open Bwd
open BwdNotation

module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type cell = {tm : D.t; tp : D.t}

type env = {
  blessed_ulvl : D.t;
  local_names : (cell, unit) Yuujinchou.Trie.t;
  locals : D.t Lazy.t bwd;
  size : int;
}

let top_env = {
  blessed_ulvl = D.lvl 0;
  local_names = Yuujinchou.Trie.empty;
  locals = Emp #< (Lazy.from_val @@ D.lvl 0);
  size = 1;
}

type resolve_data =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm: NbE.Domain.t Lazy.t; tp: NbE.Domain.t}

module Internal =
struct
  exception%effect Resolve : Yuujinchou.Trie.path -> resolve_data
  let resolve p = Effect.perform (Resolve p)

  type error =
    | NotInferable of {tm: Syntax.t}
    | IllTyped of {tm: Syntax.t; tp: D.t}
  exception Error of error
  let not_inferable ~tm = raise (Error (NotInferable {tm}))
  let ill_typed ~tm ~tp = raise (Error (IllTyped {tm; tp}))
  let trap f = try Result.ok (f ()) with Error e -> Result.error e

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)
  (* invariant: the return values must be effect-less *)
  let eval tm = NbE.eval ~env:(Eff.read()).locals tm
  let lazy_eval tm =
    let env = (Eff.read()).locals in
    lazy begin NbE.eval ~env tm end
  let quote v = NbE.quote ~size:(Eff.read()).size v
  let equate v = NbE.equate ~size:(Eff.read()).size v
  let resolve_local p = Yuujinchou.Trie.find_singleton p (Eff.read()).local_names
  let bind ~name ~tp f =
    let arg = D.lvl (Eff.read()).size in
    Eff.scope (fun env ->
        {blessed_ulvl = env.blessed_ulvl;
         size = env.size + 1;
         locals = env.locals #< (Lazy.from_val arg);
         local_names =
           match name with
           | None -> env.local_names
           | Some name -> Yuujinchou.Trie.update_singleton name (fun _ -> Some ({tm = arg; tp}, ())) env.local_names})
    @@ fun () -> f arg
  let blessed_ulvl () = (Eff.read()).blessed_ulvl
end

open Internal

let elab_shift =
  let open NbE.ULvl.Shift in
  function
  | CS.Translate i -> trans i

let shifted_blessed_ulvl =
  function
  | None -> blessed_ulvl ()
  | Some ss ->
    List.fold_right (fun s l -> D.ULvl.shifted l (elab_shift s)) ss (blessed_ulvl ())

let app_ulvl tp ulvl =
  match NbE.force_all tp with
  | D.VirPi (D.TpULvl, fam) ->
    NbE.inst_clo' fam ulvl
  | _ -> invalid_arg "app_ulvl"

let infer_var p s =
  match resolve_local p, s with
  | Some ({tm; tp}, ()), None -> quote tm, tp
  | Some _, Some _ ->
    Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
    not_inferable ~tm:{node = CS.Var (p, s); info = None}
  | None, _ ->
    let ulvl = shifted_blessed_ulvl s in
    let tm, tp =
      match resolve p with
      | Axiom {tp} -> S.axiom p, tp
      | Def {tp; tm} -> S.def p tm, tp
    in
    S.app tm (quote ulvl), app_ulvl tp ulvl

let rec infer tm =
  match tm.CS.node with
  | CS.Ann {tm; tp} ->
    let tp = eval @@ check ~tp:D.univ_top tp in
    check ~tp tm, tp
  | CS.Var (p, s) -> infer_var p s
  | CS.App (tm1, tm2) ->
    begin
      let tm1, tp1 = infer tm1 in
      match NbE.force_all tp1 with
      | D.Pi (base, fam) | D.VirPi (base, fam) ->
        let tm2 = check ~tp:base tm2 in
        let tp = NbE.inst_clo fam @@ lazy_eval tm2 in
        S.app tm1 tm2, tp
      | _ -> invalid_arg "infer"
    end
  | CS.Fst tm ->
    begin
      let tm, tp = infer tm in
      match NbE.force_all tp with
      | D.Sigma (base, _) ->
        S.fst tm, base
      | _ -> invalid_arg "infer"
    end
  | CS.Snd tm ->
    begin
      let tm, tp = infer tm in
      match NbE.force_all tp with
      | D.Sigma (_, fam) ->
        let tp = NbE.inst_clo fam @@ lazy_eval @@ S.fst tm in
        S.snd tm, tp
      | _ -> invalid_arg "infer"
    end
  | _ ->
    (* Format.eprintf "@[<2>Could@ not@ infer@ the@ type@ of@ %a@]@." Syntax.dump tm; *)
    not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm ~tp  =
  match tm.CS.node, tp with
  | CS.Pi (base, name, fam), D.Univ _ ->
    let base = check ~tp base in
    let fam = bind ~name ~tp:(eval base) @@ fun _ -> check ~tp fam
    in
    S.pi base fam
  | CS.Lam (name, body), (D.Pi (base, fam) | D.VirPi (base, fam)) ->
    bind ~name ~tp:base @@ fun arg -> S.lam @@ check ~tp:(NbE.inst_clo' fam arg) body
  | CS.Sigma (base, name, fam), D.Univ _ ->
    let base = check ~tp base in
    let fam = bind ~name ~tp:(eval base) @@ fun _ -> check ~tp fam
    in
    S.sigma base fam
  | CS.Pair (tm1, tm2), D.Sigma (base, fam) ->
    let tm1 = check ~tp:base tm1 in
    let tp2 = NbE.inst_clo fam @@ lazy_eval tm1 in
    let tm2 = check ~tp:tp2 tm2 in
    S.pair tm1 tm2
  | CS.Univ s, D.Univ large ->
    let vsmall = shifted_blessed_ulvl s in
    if UL.(<) (UL.of_con vsmall) (UL.of_con large)
    then S.univ (quote vsmall)
    else begin
      Format.eprintf "@[<2>Universe@ level@ %a@ is@ not@ smaller@ than@ %a@]@."
        (Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int) (UL.of_con vsmall)
        (Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int) (UL.of_con large);
      ill_typed ~tm ~tp
    end
  | CS.VirPi (base, name, fam), D.Univ _ ->
    let base = check ~tp:D.vir_univ base in
    let fam = bind ~name ~tp:(eval base) @@ fun _ -> check ~tp fam
    in
    S.pi base fam
  | _ ->
    if fallback_infer then
      try
        match infer tm with
        | tm', tp' ->
          begin
            try equate tp' `LE tp; tm' with
            | NbE.Unequal -> ill_typed ~tm ~tp
          end
      with Error (NotInferable _) ->
      match tp with
      | D.Unfold _ ->
        check ~fallback_infer:false tm ~tp:(NbE.force_all tp)
      | _ ->
        ill_typed ~tm ~tp
    else
      ill_typed ~tm ~tp

(* the public interface *)

type error = Internal.error =
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: D.t}
let infer_top tm =
  trap @@ fun () ->
  let tm, tp =
    Eff.run ~env:top_env @@ fun () ->
    let tm, tp = infer tm in tm, quote tp
  in
  S.lam tm, NbE.eval_top (S.vir_pi S.tp_ulvl tp)
let check_tp_top tp =
  trap @@ fun () ->
  let tp = Eff.run ~env:top_env @@ fun () -> check tp ~tp:D.univ_top in
  S.vir_pi S.tp_ulvl tp
let check_top tm ~tp =
  trap @@ fun () ->
  let tm = Eff.run ~env:top_env @@ fun () -> check tm ~tp:(app_ulvl tp @@ blessed_ulvl()) in
  S.lam tm

type handler = { resolve : Yuujinchou.Trie.path -> resolve_data }

let run f h =
  try f () with [%effect? Resolve p, k] -> Algaeff.Fun.Deep.finally k (fun () -> h.resolve p)

let perform : handler = { resolve = Internal.resolve }
