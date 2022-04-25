open Bwd
open BwdNotation

module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type cell = {tm : D.t; tp : D.t}

type env = {
  lookup : cell Yuujinchou.Trie.t;
  locals : D.t Lazy.t bwd;
  size : int;
}

let empty_env = {
  lookup = Yuujinchou.Trie.empty;
  locals = Emp;
  size = 0;
}

module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

exception NotInScope
exception IllTyped

open struct
  (* invariant: the return values must be effect-less *)

  let eval tm = NbE.eval ~locals:(Eff.read()).locals tm
  let lazy_eval tm =
    let locals = (Eff.read()).locals in
    lazy begin NbE.eval ~locals tm end
  let quote v = NbE.quote ~size:(Eff.read()).size v
  let equate v = NbE.equate ~size:(Eff.read()).size v
  let resolve_local p = Yuujinchou.Trie.find_singleton p (Eff.read()).lookup
  let bind ~name ~tp f =
    let arg = D.lvl (Eff.read()).size in
    Eff.scope (fun env ->
        {size = env.size + 1;
         locals = env.locals #< (Lazy.from_val arg);
         lookup =
           match name with
           | None -> env.lookup
           | Some name -> Yuujinchou.Trie.update_singleton name (fun _ -> Some {tm = arg; tp}) env.lookup})
    @@ fun () -> f arg
end

let rec infer tm =
  match tm.CS.node with
  | CS.Ann {tm; tp} ->
    let tp = eval @@ check ~tp:(D.Univ D.ULvl.top) tp in
    check ~tp tm, tp
  | CS.Var p ->
    begin
      match resolve_local p with
      | Some {tm; tp} -> quote tm, tp
      | None ->
        match Scope.resolve p with
        | Some Scope.Def {tp; tm} -> S.def p tm, tp
        | None -> raise NotInScope
    end
  | CS.App (tm1, tm2) ->
    begin
      let tm1, tp1 = infer tm1 in
      match NbE.force_all tp1 with
      | D.Pi (base, fam) ->
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
  | CS.ULvlTop -> S.ULvl.top, D.TpULvl
  | CS.ULvlShifted (l, s) ->
    let l = check ~tp:D.TpULvl l in
    let s = Mugenjou.Shift.Gapped.of_prefix s in
    S.ULvl.shifted l s, D.TpULvl
  | _ -> raise IllTyped

and check tm ~tp =
  match tm.CS.node, tp with
  | CS.Pi (base, name, fam), D.Univ _ ->
    let base = check ~tp base
    and fam = bind ~name ~tp @@ fun _ -> check ~tp fam
    in
    S.pi base fam
  | CS.Lam (name, body), (D.Pi (base, fam) | D.VirPi (base, fam)) ->
    bind ~name ~tp:base @@ fun arg -> S.lam @@ check ~tp:(NbE.inst_clo' fam arg) body
  | CS.Sigma (base, name, fam), D.Univ _ ->
    let base = check ~tp base
    and fam = bind ~name ~tp @@ fun _ -> check ~tp fam
    in
    S.sigma base fam
  | CS.Pair (tm1, tm2), D.Sigma (base, fam) ->
    let tm1 = check ~tp:base tm1 in
    let tp2 = NbE.inst_clo fam @@ lazy_eval tm1 in
    let tm2 = check ~tp:tp2 tm2 in
    S.pair tm1 tm2
  | CS.Univ small, D.Univ large ->
    let small = check ~tp:D.TpULvl small in
    let vsmall = eval small in
    if UL.(<) (UL.of_con vsmall) (UL.of_con large) then S.univ small else raise IllTyped
  | CS.VirPi (base, name, fam), D.Univ _ ->
    let base = check ~tp:D.VirUniv base
    and fam = bind ~name ~tp @@ fun _ -> check ~tp fam
    in
    S.pi base fam
  | CS.TpULvl, D.VirUniv ->
    S.TpULvl
  | _ ->
    let tm, tp' = infer tm in
    equate tp' `LE tp;
    tm

let infer_top tm = Eff.run ~env:empty_env @@ fun () -> infer tm
let check_top tm ~tp = Eff.run ~env:empty_env @@ fun () -> check tm ~tp
