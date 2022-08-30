module Syntax = Syntax
module Eff = Eff

module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl
module R = Refiner
module T = R.Tactic

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
    Eff.unleash None @@ R.ResolveData.Axiom {tp}
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
    Error.Logger.fatalf ?loc:(R.Eff.loc ()) ~code:NotInferable "@[<2>Local@ variable@ '%a'@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p
  | None, _ ->
    R.Structural.global_var p (check_shift s)

let rec infer tm : T.infer =
  let open Asai.Span in
  T.Infer.locate ~loc:tm.loc @@
  T.Infer.trace ?loc:tm.loc "While inferring" @@
  match tm.value with
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
    Error.Logger.fatalf ?loc:tm.loc ~code:NotInferable "Could not infer a type for this term"

and check tm : T.check =
  let open Asai.Span in
  T.Check.locate ~loc:tm.loc @@
  T.Check.peek @@ fun goal ->
  T.Check.trace ?loc:tm.loc (Format.asprintf "While checking against %a" S.dump (R.Eff.quote goal.tp)) @@
  match tm.value with
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
  | _ -> T.Check.infer (infer tm)


(* the public interface *)

let infer_top lhs tm =
  let tm, tp =
    R.Eff.with_top_env @@ fun () ->
    let tm, tp = T.Infer.run {lhs} @@ infer tm in
    tm, R.Eff.quote tp
  in
  S.lam tm, NbE.eval_top @@ S.vir_pi S.tp_ulvl tp

let check_tp_top lhs tp =
  let tp =
    R.Eff.with_top_env @@ fun () ->
    T.Check.run {tp = D.univ_top; lhs} @@ check tp
  in
  S.vir_pi S.tp_ulvl tp

let check_top lhs tm ~tp =
  S.lam @@
  R.Eff.with_top_env @@ fun () ->
  let ulvl = T.Shift.run @@ R.ULvl.base in
  T.Check.run {tp = NbE.app_ulvl ~tp ~ulvl; lhs} @@ check tm
