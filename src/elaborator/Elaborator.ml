module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

module Syntax = CS
module Errors = Errors
module ResolveData = ResolveData

type infer = unit -> S.t * D.t
type shift = unit -> D.t
type check = tp:D.t -> S.t
type 'a binder = D.t -> 'a

module Structural : sig
  val local_var : RefineEffect.cell -> infer
  val global_var : Yuujinchou.Trie.path -> shift -> infer
  val ann : ctp:check -> ctm:check -> infer
end =
struct

  let local_var cell = fun () ->
    RefineEffect.quote cell.RefineEffect.tm, cell.RefineEffect.tp

  let global_var path shift = fun () ->
    let ulvl = shift () in
    let tm, tp =
      match RefineEffect.resolve path with
      | ResolveData.Axiom {tp} -> S.axiom path, tp
      | ResolveData.Def {tp; tm} -> S.def path tm, tp
    in
    S.app tm (RefineEffect.quote ulvl), NbE.app_ulvl ~tp ~ulvl

  let ann ~ctp ~ctm : infer = fun () ->
    let tp = RefineEffect.eval @@ ctp ~tp:D.univ_top in
    ctm ~tp, tp

end

module Quantifier : sig
  type rule = name:CS.bound_name -> cbase:check -> cfam:check binder -> (S.t -> S.t -> S.t) -> check

  val quantifier : rule
  val vir_quantifier : rule
end =
struct

  type rule = name:CS.bound_name -> cbase:check -> cfam:check binder -> (S.t -> S.t -> S.t) -> check

  let quantifier ~name ~cbase ~cfam syn : check = fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = cbase ~tp in
      let fam = RefineEffect.bind ~name ~tp:(RefineEffect.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

  let vir_quantifier ~name ~cbase ~cfam syn : check = fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = cbase ~tp:D.VirUniv in
      let fam = RefineEffect.bind ~name ~tp:(RefineEffect.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

end

module Sigma : sig
  val sigma : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val pair : cfst:check -> csnd:check -> check
  val fst : itm:infer -> infer
  val snd : itm:infer -> infer
end =
struct

  let sigma ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.sigma

  let pair ~cfst ~csnd : check = fun ~tp ->
    match tp with
    | D.Sigma (base, fam) ->
      let tm1 = cfst ~tp:base in
      let tp2 = NbE.inst_clo fam @@ RefineEffect.lazy_eval tm1 in
      let tm2 = csnd ~tp:tp2 in
      S.pair tm1 tm2
    | _ ->
      invalid_arg "pair"

  let fst ~itm : infer = fun () ->
    let tm, tp = itm () in
    match NbE.force_all tp with
    | D.Sigma (base, _) ->
      S.fst tm, base
    | _ ->
      invalid_arg "fst"

  let snd ~itm : infer = fun () ->
    let tm, tp = itm () in
    match NbE.force_all tp with
    | D.Sigma (_, fam) ->
      let tp = NbE.inst_clo fam @@ RefineEffect.lazy_eval @@ S.fst tm in
      S.snd tm, tp
    | _ ->
      invalid_arg "snd"

end

module Pi : sig
  val pi : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val vir_pi : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val lam : name:CS.bound_name -> cbnd:check binder -> check
  val app : itm:infer -> ctm:check -> infer
end =
struct

  let pi ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.pi

  let vir_pi ~name ~cbase ~cfam : check =
    Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

  let lam ~name ~cbnd : check = fun ~tp ->
    match tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      RefineEffect.bind ~name ~tp:base @@ fun arg ->
      S.lam @@ cbnd arg ~tp:(NbE.inst_clo' fam arg)
    | _ ->
      failwith ""

  let app ~itm ~ctm : infer = fun () ->
    let fn, fn_tp = itm () in
    match NbE.force_all fn_tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      let arg = ctm ~tp:base in
      let fib = NbE.inst_clo fam @@ RefineEffect.lazy_eval arg in
      S.app fn arg, fib
    | _ ->
      invalid_arg "app"

end

module Univ : sig
  val univ : shift -> check
end =
struct
  let univ shift ~tp =
    match tp with
    | D.Univ large ->
      let vsmall = shift () in
      if UL.(<) (UL.of_con vsmall) (UL.of_con large)
      then S.univ (RefineEffect.quote vsmall)
      else begin
        let pp_lvl = Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int in
        Format.eprintf "@[<2>Universe@ level@ %a@ is@ not@ smaller@ than@ %a@]@."
          pp_lvl (UL.of_con vsmall)
          pp_lvl (UL.of_con large);
        invalid_arg "univ"
      end
    | _ ->
      invalid_arg "univ"
end


let check_shift (s : Syntax.shift list option) : shift =
  match s with
  | None ->
    RefineEffect.blessed_ulvl
  | Some ss ->
    fun () ->
      List.fold_right
        (fun (CS.Translate i) l -> D.ULvl.shifted l @@ NbE.ULvl.Shift.of_int i)
        ss
        (RefineEffect.blessed_ulvl ())

let infer_var p s : infer = fun () ->
  match RefineEffect.resolve_local p, s with
  | Some (cell, ()), None -> Structural.local_var cell ()
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    RefineEffect.not_inferable ~tm:{node = CS.Var (p, s); loc = None}
  | None, _ ->
    Structural.global_var p (check_shift s) ()

let rec infer tm : infer = fun () ->
  match tm.CS.node with
  | CS.Var (p, s) ->
    infer_var p s ()
  | CS.Ann {tm; tp} ->
    Structural.ann ~ctp:(check tp) ~ctm:(check tm) ()
  | CS.App (tm1, tm2) ->
    Pi.app ~itm:(infer tm1) ~ctm:(check tm2) ()
  | CS.Fst tm ->
    Sigma.fst ~itm:(infer tm) ()
  | CS.Snd tm ->
    Sigma.snd ~itm:(infer tm) ()
  | _ ->
    (* Format.eprintf "@[<2>Could@ not@ infer@ the@ type@ of@ %a@]@." Syntax.dump tm; *)
    RefineEffect.not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm : check = fun ~tp ->
  match tm.CS.node with
  | CS.Pi (base, name, fam) ->
    Pi.pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.VirPi (base, name, fam) ->
    Pi.vir_pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Sigma (base, name, fam) ->
    Sigma.sigma ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Lam (name, body) ->
    Pi.lam ~name ~cbnd:(fun _ -> check body) ~tp
  | CS.Pair (tm1, tm2) ->
    Sigma.pair ~cfst:(check tm1) ~csnd:(check tm2) ~tp
  | CS.Univ s ->
    Univ.univ (check_shift s) ~tp
  | _ when fallback_infer ->
    begin
      try
        match infer tm () with
        | tm', tp' ->
          begin
            try RefineEffect.equate tp' `LE tp; tm' with
            | NbE.Unequal -> RefineEffect.ill_typed ~tm ~tp
          end
      with RefineEffect.Error (NotInferable _) ->
      match tp with
      | D.Unfold _ ->
        check ~fallback_infer:false tm ~tp:(NbE.force_all tp)
      | _ ->
        RefineEffect.ill_typed ~tm ~tp
    end
  | _ ->
    RefineEffect.ill_typed ~tm ~tp

(* the public interface *)

let infer_top tm =
  RefineEffect.trap @@ fun () ->
  let tm, tp =
    RefineEffect.with_top_env @@ fun () ->
    let tm, tp = infer tm () in tm, RefineEffect.quote tp
  in
  S.lam tm, NbE.eval_top (S.vir_pi S.tp_ulvl tp)

let check_tp_top tp =
  RefineEffect.trap @@ fun () ->
  let tp = RefineEffect.with_top_env @@ fun () -> check tp ~tp:D.univ_top in
  S.vir_pi S.tp_ulvl tp

let check_top tm ~tp =
  RefineEffect.trap @@ fun () ->
  S.lam @@
  RefineEffect.with_top_env @@ fun () ->
  check tm ~tp:(NbE.app_ulvl ~tp ~ulvl:(RefineEffect.blessed_ulvl ()))

type handler = RefineEffect.handler = { resolve : CS.name -> ResolveData.t }
let run = RefineEffect.run
let perform = RefineEffect.perform
