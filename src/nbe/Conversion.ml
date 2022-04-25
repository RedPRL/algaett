open Bwd

module D = Domain

let rec force_all =
  function
  | D.Unfold (_, _, v) -> force_all (Lazy.force v)
  | v -> v

type env =
  { mode : [`Rigid | `Flex | `Full];
    size : int }

module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

let with_mode s f =
  Eff.scope (fun env -> {env with mode = s}) f

let flip =
  function
  | `LE -> `GE
  | `EQ -> `EQ
  | `GE -> `LE

let force x =
  match (Eff.read()).mode with
  | `Full -> force_all x
  | _ -> x

let bind f =
  let arg = D.lvl (Eff.read()).size in
  Eff.scope (fun env -> {env with size = env.size + 1}) @@ fun () ->
  f arg

exception Unequal

let equal_unfold_hd h1 h2 =
  match h1, h2 with
  | D.Def (p1, _), D.Def (p2, _) -> List.equal String.equal p1 p2

let equate_cut_hd h1 h2 =
  match h1, h2 with
  | D.Lvl l1, D.Lvl l2 when Int.equal l1 l2 -> ()
  | D.Axiom p1, D.Axiom p2 when List.equal String.equal p1 p2 -> ()
  | _ -> raise Unequal

let equate_ulvl' l1 dir l2 =
  if (match dir with
      | `GE -> ULvl.ge
      | `EQ -> ULvl.equal
      | `LE -> ULvl.le)
      (ULvl.of_con l1) (ULvl.of_con l2)
  then () else raise Unequal

let equate_ulvl l1 l2 =
  if ULvl.equal (ULvl.of_endo l1) (ULvl.of_endo l2) then () else raise Unequal

let rec equate v1 dir v2 =
  match force v1, dir, force v2, (Eff.read()).mode with

  (* rigid cases *)
  | D.Cut (h1, sp1), _, D.Cut (h2, sp2), _ ->
    equate_cut_hd h1 h2;
    equate_spine sp1 sp2
  | D.Pi (b1, f1), dir, D.Pi (b2, f2), _ ->
    equate b1 (flip dir) b2;
    equate_clo f1 dir f2
  | D.Lam c1, _, D.Lam c2, _ ->
    equate_clo c1 `EQ c2
  | D.Sigma (b1, f1), dir, D.Sigma (b2, f2), _ ->
    equate b1 dir b2;
    equate_clo f1 dir f2
  | D.Pair (fst1, snd1), _, D.Pair (fst2, snd2), _ ->
    equate fst1 `EQ fst2;
    equate snd1 `EQ snd2
  | D.Univ l1, dir, D.Univ l2, _ ->
    equate_ulvl' l1 dir l2
  | D.VirPi (b1, f1), dir, D.VirPi (b2, f2), _ ->
    equate b1 (flip dir) b2;
    equate_clo f1 dir f2
  | D.TpULvl, _, D.TpULvl, _ -> ()
  | D.ULvl l1, _, D.ULvl l2, _ ->
    equate_ulvl l1 l2
  | D.VirUniv, _, D.VirUniv, _ -> ()

  (* unfolding *)
  | D.Unfold (h1, sp1, _), _, D.Unfold (h2, sp2, _), `Flex when equal_unfold_hd h1 h2 ->
    equate_spine sp1 sp2
  | D.Unfold _, _, _, `Flex | _, _, D.Unfold _, `Flex ->
    raise Unequal
  | D.Unfold (h1, sp1, v1), dir, D.Unfold (h2, sp2, v2), `Rigid ->
    if equal_unfold_hd h1 h2 then
      try
        with_mode `Flex @@ fun () -> equate_spine sp1 sp2
      with Unequal ->
        (* in the smalltt, the mode is changed to `Full directly. why not `Rigid? *)
        with_mode `Full @@ fun () -> equate (Lazy.force v1) dir (Lazy.force v2)
    else
      equate (Lazy.force v1) dir (Lazy.force v2)
  | D.Unfold (_, _, v1), dir, v2, `Rigid ->
    equate (Lazy.force v1) dir v2
  | v1, dir, D.Unfold (_, _, v2), `Rigid ->
    equate v1 dir (Lazy.force v2)
  | D.Unfold _, _, _, `Full | _, _, D.Unfold _, `Full ->
    failwith "imposseble: force did not unfold all Domain.Unfold"

  (* eta expansion *)
  | v1, _, v2, _ -> equate_cold v1 v2

and equate_cold v1 v2 =
  match v1, v2 with
  | D.Cut (hd, sp), D.Lam clo | D.Lam clo, D.Cut (hd, sp) ->
    bind @@ fun arg -> equate_spine_cold (hd, Snoc (sp, D.App arg)) (Semantics.inst_clo' clo arg)
  | D.Cut (hd, sp), D.Pair (v1, v2) | D.Pair (v1, v2), D.Cut (hd, sp) ->
    equate_spine_cold (hd, Snoc (sp, D.Fst)) v1;
    equate_spine_cold (hd, Snoc (sp, D.Snd)) v2
  | _ -> raise Unequal

and equate_spine_cold (hd1, sp1) v =
  match force v with
  | D.Cut (hd2, sp2) when hd1 = hd2 -> equate_spine sp1 sp2
  | D.Cut _ -> raise Unequal
  | D.Lam clo -> bind @@ fun arg ->
    equate_spine_cold (hd1, Snoc (sp1, D.App arg)) (Semantics.inst_clo' clo arg)
  | D.Pair (v1, v2) ->
    equate_spine_cold (hd1, Snoc (sp1, D.Fst)) v1;
    equate_spine_cold (hd1, Snoc (sp1, D.Snd)) v2
  | _ -> raise Unequal

and equate_frm f1 f2 =
  match f1, f2 with
  | D.App v1, D.App v2 -> equate v1 `EQ v2
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

and equate_clo c1 dir c2 =
  bind @@ fun arg -> equate (Semantics.inst_clo' c1 arg) dir (Semantics.inst_clo' c2 arg)

let equate ~size v1 dir v2 = Eff.run ~env:{mode = `Rigid; size} @@ fun () -> equate v1 dir v2
