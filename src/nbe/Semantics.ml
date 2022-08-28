open Bwd
open Bwd.Infix

module S = Syntax
module D = Domain

module Internal =
struct
  type env = D.env
  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let make_clo body = D.Clo {body; env = Eff.read()}

  let of_idx idx = BwdLabels.nth (Eff.read()) idx

  let rec inst_clo (D.Clo {body; env}) arg : D.t =
    let env = env #< arg in
    Eff.run ~env @@ fun () -> eval body

  and inst_clo' clo arg = inst_clo clo @@ SyncLazy.from_val arg

  and app v0 v1 =
    match v0 with
    | D.Lam clo -> inst_clo' clo v1
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< (D.App v1))
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< (D.App v1), SyncLazy.map (fun v0 -> app v0 v1) v0)
    | _ -> invalid_arg "Evaluation.app"

  and fst : D.t -> D.t =
    function
    | D.Pair (v0, _) -> v0
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Fst)
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< D.Fst, SyncLazy.map fst v0)
    | _ -> invalid_arg "Evaluation.fst"

  and snd : D.t -> D.t =
    function
    | D.Pair (_, v1) -> v1
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Snd)
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< D.Snd, SyncLazy.map snd v0)
    | _ -> invalid_arg "Evaluation.snd"

  and eval_ulvl =
    let module M = Mugen.Syntax in
    function
    | M.Top -> D.ULvl.top
    | M.Shifted (ulvl, s) -> D.ULvl.shifted (eval ulvl) s

  and eval : S.t -> D.t =
    function
    | S.Var idx -> SyncLazy.force (of_idx idx)
    | S.Axiom p -> D.Cut (D.Axiom p, Emp)
    | S.Def (p, v) -> D.def p v
    | S.Pi (base, (* binding *) fam) -> D.Pi (eval base, make_clo fam)
    | S.Lam (* binding *) body -> D.Lam (make_clo body)
    | S.App (tm0, tm1) -> app (eval tm0) (eval tm1)
    | S.Sigma (base, (* binding *) fam) -> D.Sigma (eval base, make_clo fam)
    | S.Pair (t0, t1) -> D.Pair (eval t0, eval t1)
    | S.Fst t -> fst (eval t)
    | S.Snd t -> snd (eval t)
    | S.Univ t -> D.Univ (eval t)
    | S.VirPi (base, (* binding *) fam) -> D.VirPi (eval base, make_clo fam)
    | S.TpULvl -> D.TpULvl
    | S.ULvl l -> eval_ulvl l
    | S.VirUniv -> D.VirUniv
end

let inst_clo = Internal.inst_clo
let inst_clo' = Internal.inst_clo'
let eval ~env tm = Internal.Eff.run ~env @@ fun () -> Internal.eval tm
let eval_top tm = eval ~env:Emp tm
