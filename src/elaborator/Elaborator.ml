module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

module Syntax = CS
module Errors = Errors
module ResolveData = ResolveData

module R = Refiner

let check_shift (s : CS.shift list option) : R.shift =
  match s with
  | None ->
    R.Shift.blessed
  | Some ss ->
    List.fold_right
      (fun (CS.Translate i) l -> R.Shift.shifted l i)
      ss
      R.Shift.blessed

let infer_var p s : R.infer =
  match RefineEffect.resolve_local p, s with
  | Some (cell, ()), None ->
    R.Structural.local_var cell
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    RefineEffect.not_inferable ~tm:{node = CS.Var (p, s); loc = None}
  | None, _ ->
    R.Structural.global_var p (check_shift s)

let rec infer tm : R.infer =
  match tm.CS.node with
  | CS.Var (p, s) ->
    infer_var p s
  | CS.Ann {tm; tp} ->
    R.Structural.ann ~ctp:(check tp) ~ctm:(check tm)
  | CS.App (tm1, tm2) ->
    R.Pi.app ~itm:(infer tm1) ~ctm:(check tm2)
  | CS.Fst tm ->
    R.Sigma.fst ~itm:(infer tm)
  | CS.Snd tm ->
    R.Sigma.snd ~itm:(infer tm)
  | _ ->
    (* Format.eprintf "@[<2>Could@ not@ infer@ the@ type@ of@ %a@]@." Syntax.dump tm; *)
    RefineEffect.not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm : R.check =
  R.Check.peek @@ fun ~tp ->
  match tm.CS.node with
  | CS.Pi (base, name, fam) ->
    R.Pi.pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam)
  | CS.VirPi (base, name, fam) ->
    R.Pi.vir_pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam)
  | CS.Sigma (base, name, fam) ->
    R.Sigma.sigma ~name ~cbase:(check base) ~cfam:(fun _ -> check fam)
  | CS.Lam (name, body) ->
    R.Pi.lam ~name ~cbnd:(fun _ -> check body)
  | CS.Pair (tm1, tm2) ->
    R.Sigma.pair ~cfst:(check tm1) ~csnd:(check tm2)
  | CS.Univ s ->
    R.Univ.univ (check_shift s)
  | _ when fallback_infer ->
    begin
      R.Check.peek @@ fun ~tp ->
      R.Check.orelse (R.Check.infer (infer tm)) @@ fun err ->
      match err, tp with
      | NotInferable _, D.Unfold _ ->
        R.Check.forcing @@ check ~fallback_infer:false tm
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
    let tm, tp = R.Infer.run @@ infer tm in tm, RefineEffect.quote tp
  in
  S.lam tm, NbE.eval_top (S.vir_pi S.tp_ulvl tp)

let check_tp_top tp =
  RefineEffect.trap @@ fun () ->
  let tp = RefineEffect.with_top_env @@ fun () -> R.Check.run ~tp:D.univ_top @@ check tp in
  S.vir_pi S.tp_ulvl tp

let check_top tm ~tp =
  RefineEffect.trap @@ fun () ->
  S.lam @@
  RefineEffect.with_top_env @@ fun () ->
  R.Check.run ~tp:(NbE.app_ulvl ~tp ~ulvl:(RefineEffect.blessed_ulvl ())) @@ check tm

type handler = RefineEffect.handler = { resolve : CS.name -> ResolveData.t }
let run = RefineEffect.run
let perform = RefineEffect.perform
