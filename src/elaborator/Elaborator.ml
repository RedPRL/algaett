module Syntax = Syntax
module Errors = Errors

module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl
module R = Refiner
module T = R.Tactic

exception Error of Errors.t

type _ Effect.t += Unleash : CS.bound_name * R.ResolveData.t -> CS.name Effect.t
let unleash p data = Effect.perform @@ Unleash (p, data)

let not_inferable ~tm = raise @@ Error (NotInferable {tm})
let ill_typed ~tm ~tp = raise @@ Error (IllTyped {tm; tp})

let unleash_hole : T.check =
  T.Check.peek @@ fun goal ->
  let bnds = R.Eff.Generalize.quote_ctx () in

  let p =
    let make_pi bnd bdy =
      match bnd with
      | R.Eff.Generalize.VirType tp -> S.VirPi (tp, bdy)
      | R.Eff.Generalize.Type tp -> S.Pi (tp, bdy)
    in
    let tp =
      R.Eff.with_top_env @@ fun () ->
      R.Eff.eval @@
      List.fold_right make_pi bnds @@ R.Eff.quote goal.tp
    in
    unleash None @@ R.ResolveData.Axiom {tp}
  in

  T.Check.infer @@
  let head = R.Structural.global_var p @@ R.ULvl.base in
  let app _ (l, itm) = l + 1, R.Pi.app ~itm ~ctm:(T.Check.infer @@ R.Structural.level l) in
  snd @@ List.fold_right app bnds (1, head)


let check_shift (s : CS.shift list option) : T.shift =
  match s with
  | None ->
    R.ULvl.base
  | Some ss ->
    List.fold_right
      (fun (CS.Translate i) l -> R.ULvl.shifted l i)
      ss
      R.ULvl.base

let infer_var p s : T.infer =
  match R.Eff.resolve_local p, s with
  | Some (cell, ()), None ->
    R.Structural.local_var cell
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    not_inferable ~tm:{node = CS.Var (p, s); loc = None}
  | None, _ ->
    R.Structural.global_var p (check_shift s)

let rec infer tm : T.infer =
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
    not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm : T.check =
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
  | CS.Hole ->
    unleash_hole
  | _ when fallback_infer ->
    begin
      T.Check.peek @@ fun goal ->
      T.Check.orelse (T.Check.infer (infer tm)) @@ fun exn ->
      match exn, goal.tp with
      | Error NotInferable _, D.Unfold _ ->
        T.Check.forcing @@ check ~fallback_infer:false tm
      | _ ->
        ill_typed ~tm ~tp:goal.tp
    end
  | _ ->
    T.Check.peek @@ fun goal ->
    ill_typed ~tm ~tp:goal.tp


(* the public interface *)

let trap (f : unit -> 'a) : ('a, Errors.t) Result.t =
  R.Eff.trap f |> Result.map_error @@ function
  | R.Errors.Conversion (u, v) -> Errors.Conversion (u, v)

let infer_top lhs tm =
  trap @@ fun () ->
  let tm, tp =
    R.Eff.with_top_env @@ fun () ->
    let tm, tp = T.Infer.run {lhs} @@ infer tm in
    tm, R.Eff.quote tp
  in
  S.lam tm, NbE.eval_top @@ S.vir_pi S.tp_ulvl tp

let check_tp_top lhs tp =
  trap @@ fun () ->
  let tp =
    R.Eff.with_top_env @@ fun () ->
    T.Check.run {tp = D.univ_top; lhs} @@ check tp
  in
  S.vir_pi S.tp_ulvl tp

let check_top lhs tm ~tp =
  trap @@ fun () ->
  S.lam @@
  R.Eff.with_top_env @@ fun () ->
  let ulvl = T.Shift.run @@ R.ULvl.base in
  T.Check.run {tp = NbE.app_ulvl ~tp ~ulvl; lhs} @@ check tm

module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : CS.bound_name -> Refiner.ResolveData.t -> CS.name
end

module Run (H : Handler) =
struct
  let run_refiner f =
    let module Run = R.Eff.Run (H) in
    Run.run f

  let run f =
    Effect.Deep.try_with
      run_refiner f
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Unleash (p, rdata) ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> H.unleash p rdata
            | _ -> None }
end

module Perform : Handler =
struct
  include R.Eff.Perform
  let unleash = unleash
end
