module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

module Syntax = CS
module Errors = Errors
module ResolveData = ResolveData

module R = Refiner

let check_shift (s : CS.shift list option) : Rule.shift =
  match s with
  | None ->
    R.Shift.base
  | Some ss ->
    List.fold_right
      (fun (CS.Translate i) l -> R.Shift.shifted l i)
      ss
      R.Shift.base

let infer_var p s : Rule.infer =
  match RefineEffect.resolve_local p, s with
  | Some (cell, ()), None ->
    R.Structural.local_var cell
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    RefineEffect.not_inferable ~tm:{node = CS.Var (p, s); loc = None}
  | None, _ ->
    R.Structural.global_var p (check_shift s)

let rec infer tm : Rule.infer =
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
and check ?(fallback_infer=true) tm : Rule.check =
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
      Rule.Check.peek @@ fun goal ->
      Rule.Check.orelse (Rule.Check.infer (infer tm)) @@ fun err ->
      match err, goal.tp with
      | NotInferable _, D.Unfold _ ->
        Rule.Check.forcing @@ check ~fallback_infer:false tm
      | _ ->
        RefineEffect.ill_typed ~tm ~tp:goal.tp
    end
  | _ ->
    Rule.Check.peek @@ fun goal ->
    RefineEffect.ill_typed ~tm ~tp:goal.tp


(* the public interface *)

let infer_top lhs tm =
  RefineEffect.trap @@ fun () ->
  let tm, tp =
    RefineEffect.with_top_env @@ fun () ->
    let tm, tp = Rule.Infer.run {lhs} @@ infer tm in
    tm, RefineEffect.quote tp
  in
  S.lam tm, NbE.eval_top (S.vir_pi S.tp_ulvl tp)

let check_tp_top lhs tp =
  RefineEffect.trap @@ fun () ->
  let tp =
    RefineEffect.with_top_env @@ fun () ->
    Rule.Check.run {tp = D.univ_top; lhs} @@ check tp
  in
  S.vir_pi S.tp_ulvl tp

let check_top lhs tm ~tp =
  RefineEffect.trap @@ fun () ->
  S.lam @@
  RefineEffect.with_top_env @@ fun () ->
  let ulvl = Rule.Shift.run @@ R.Shift.base in
  Rule.Check.run {tp = NbE.app_ulvl ~tp ~ulvl; lhs} @@ check tm

type handler = RefineEffect.handler = { resolve : CS.name -> ResolveData.t }
let run = RefineEffect.run
let perform = RefineEffect.perform
