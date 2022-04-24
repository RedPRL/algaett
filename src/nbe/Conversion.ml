open Bwd

module D = Domain

type env =
  { mode : [`Rigid | `Flex | `Full];
    size : int }

module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

let with_mode s f =
  Eff.scope (fun env -> {env with mode = s}) f

let rec force_all =
  function
  | D.Unfold (_, _, v) -> force_all (Lazy.force v)
  | v -> v

let force x =
  match (Eff.read()).mode with
  | `Full -> force_all x
  | _ -> x

let bind f =
  let arg = D.lvl (Eff.read()).size in
  Eff.scope (fun env -> {env with size = env.size + 1}) @@ fun () ->
  f arg

exception Unequal

module ULvl = Mugenjou.Theory.Make
    (struct
      module Shift = Mugenjou.Shift.Gapped
      type var = int
      let equal_var = Int.equal
    end)

let equal_unfold_hd h1 h2 =
  match h1, h2 with
  | D.Global p1, D.Global p2 -> List.equal String.equal p1 p2

let equate_cut_hd h1 h2 =
  match h1, h2 with
  | D.Lvl l1, D.Lvl l2 when Int.equal l1 l2 -> ()
  | D.Axiom p1, D.Axiom p2 when List.equal String.equal p1 p2 -> ()
  | _ -> raise Unequal

(* XXX This assumes there's no Domain.Unfold within universe levels *)
let equate_ulvl l1 l2 =
  if ULvl.equal (D.ULvl.of_con l1) (D.ULvl.of_con l2) then () else raise Unequal

let rec equate v1 v2 =
  match force v1, force v2, (Eff.read()).mode with

  (* rigid cases *)
  | D.Cut (h1, sp1), D.Cut (h2, sp2), _ ->
    equate_cut_hd h1 h2;
    equate_spine sp1 sp2
  | D.Pi (b1, f1), D.Pi (b2, f2), _ ->
    equate b1 b2;
    equate_clo f1 f2
  | D.Lambda c1, D.Lambda c2, _ ->
    equate_clo c1 c2
  | D.Sigma (b1, f1), D.Sigma (b2, f2), _ ->
    equate b1 b2;
    equate_clo f1 f2
  | D.Pair (fst1, snd1), D.Pair (fst2, snd2), _ ->
    equate fst1 fst2;
    equate snd1 snd2
  | D.Univ v1, D.Univ v2, _ ->
    equate v1 v2
  | (D.ULvl _ as v1), (D.ULvl _ as v2), _ ->
    equate_ulvl v1 v2

  (* unfolding *)
  | D.Unfold (h1, sp1, _), D.Unfold (h2, sp2, _), `Flex when equal_unfold_hd h1 h2 ->
    equate_spine sp1 sp2
  | D.Unfold _, _, `Flex | _, D.Unfold _, `Flex ->
    raise Unequal
  | D.Unfold (h1, sp1, v1), D.Unfold (h2, sp2, v2), `Rigid ->
    if equal_unfold_hd h1 h2 then
      try
        with_mode `Flex @@ fun () -> equate_spine sp1 sp2
      with Unequal ->
        (* in the smalltt, the mode is changed to `Full directly. why not `Rigid? *)
        with_mode `Full @@ fun () -> equate (Lazy.force v1) (Lazy.force v2)
    else
      equate (Lazy.force v1) (Lazy.force v2)
  | D.Unfold (_, _, v1), v2, `Rigid | v2, D.Unfold (_, _, v1), `Rigid ->
    equate (Lazy.force v1) v2
  | D.Unfold _, _, `Full | _, D.Unfold _, `Full ->
    failwith "imposseble: force did not unfold all Domain.Unfold"

  (* eta expansion *)
  | v1, v2, _ -> equate_cold v1 v2

and equate_cold v1 v2 =
  match v1, v2 with
  | D.Cut (hd, sp), D.Lambda clo | D.Lambda clo, D.Cut (hd, sp) ->
    bind @@ fun arg -> equate_spine_cold (hd, Snoc (sp, D.App arg)) (Semantics.inst_clo' clo ~arg)
  | D.Cut (hd, sp), D.Pair (v1, v2) | D.Pair (v1, v2), D.Cut (hd, sp) ->
    equate_spine_cold (hd, Snoc (sp, D.Fst)) v1;
    equate_spine_cold (hd, Snoc (sp, D.Snd)) v2
  | _ -> raise Unequal

and equate_spine_cold (hd1, sp1) v =
  match force v with
  | D.Cut (hd2, sp2) when hd1 = hd2 -> equate_spine sp1 sp2
  | D.Cut _ -> raise Unequal
  | D.Lambda clo -> bind @@ fun arg ->
    equate_spine_cold (hd1, Snoc (sp1, D.App arg)) (Semantics.inst_clo' clo ~arg)
  | D.Pair (v1, v2) ->
    equate_spine_cold (hd1, Snoc (sp1, D.Fst)) v1;
    equate_spine_cold (hd1, Snoc (sp1, D.Snd)) v2
  | _ -> raise Unequal

and equate_frm f1 f2 =
  match f1, f2 with
  | D.App v1, D.App v2 -> equate v1 v2
  | D.Fst, D.Fst -> ()
  | D.Snd, D.Snd -> ()
  | _ -> raise Unequal

and equate_spine sp1 sp2 =
  match sp1, sp2 with
  | Emp, Emp -> ()
  | Snoc (sp1, frm1), Snoc (sp2, frm2) ->
    equate_spine sp1 sp2;
    equate_frm frm1 frm2
  | _ -> raise Unequal

and equate_clo c1 c2 =
  bind @@ fun arg -> equate (Semantics.inst_clo' c1 ~arg) (Semantics.inst_clo' c2 ~arg)

let equate ~size v1 v2 = Eff.run ~env:{mode = `Rigid; size} @@ fun () -> equate v1 v2
