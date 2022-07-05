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
    RefineEffect.blessed_ulvl
  | Some ss ->
    fun () ->
      List.fold_right
        (fun (CS.Translate i) l -> D.ULvl.shifted l @@ NbE.ULvl.Shift.of_int i)
        ss
        (RefineEffect.blessed_ulvl ())

let infer_var p s : R.infer = fun () ->
  match RefineEffect.resolve_local p, s with
  | Some (cell, ()), None ->
    R.Structural.local_var cell ()
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    RefineEffect.not_inferable ~tm:{node = CS.Var (p, s); loc = None}
  | None, _ ->
    R.Structural.global_var p (check_shift s) ()

let rec infer tm : R.infer = fun () ->
  match tm.CS.node with
  | CS.Var (p, s) ->
    infer_var p s ()
  | CS.Ann {tm; tp} ->
    R.Structural.ann ~ctp:(check tp) ~ctm:(check tm) ()
  | CS.App (tm1, tm2) ->
    R.Pi.app ~itm:(infer tm1) ~ctm:(check tm2) ()
  | CS.Fst tm ->
    R.Sigma.fst ~itm:(infer tm) ()
  | CS.Snd tm ->
    R.Sigma.snd ~itm:(infer tm) ()
  | _ ->
    (* Format.eprintf "@[<2>Could@ not@ infer@ the@ type@ of@ %a@]@." Syntax.dump tm; *)
    RefineEffect.not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm : R.check = fun ~tp ->
  match tm.CS.node with
  | CS.Pi (base, name, fam) ->
    R.Pi.pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.VirPi (base, name, fam) ->
    R.Pi.vir_pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Sigma (base, name, fam) ->
    R.Sigma.sigma ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Lam (name, body) ->
    R.Pi.lam ~name ~cbnd:(fun _ -> check body) ~tp
  | CS.Pair (tm1, tm2) ->
    R.Sigma.pair ~cfst:(check tm1) ~csnd:(check tm2) ~tp
  | CS.Univ s ->
    R.Univ.univ (check_shift s) ~tp
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
