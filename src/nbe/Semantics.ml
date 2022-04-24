open Bwd
open BwdNotation

module S = Syntax
module D = Domain

module Internal =
struct
  type locals = D.env
  type resolve = Yuujinchou.Trie.path -> Domain.t option
  type env = { locals : locals; resolve : resolve }
  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let make_clo body = D.Clo {body; env = (Eff.read()).locals}

  exception Unresolved of Yuujinchou.Trie.path
  let of_idx idx = BwdLabels.nth (Eff.read()).locals idx
  let of_global p =
    match (Eff.read()).resolve p with
    | Some tm -> D.Unfold (Global p, Emp, Lazy.from_val tm)
    | None -> raise (Unresolved p)

  let rec inst_clo (D.Clo {body; env}) ~arg : D.t =
    let env = {(Eff.read()) with locals = env #< arg} in
    Eff.run ~env @@ fun () -> eval body

  and inst_clo' clo ~arg = inst_clo clo ~arg:(Lazy.from_val arg)

  and app v0 v1 =
    match v0 with
    | D.Lambda clo -> inst_clo' clo ~arg:v1
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< (D.App v1))
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< (D.App v1), lazy begin app (Lazy.force v0) v1 end)
    | _ -> invalid_arg "Evaluation.app"

  and fst : D.t -> D.t =
    function
    | D.Pair (v0, _) -> v0
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Fst)
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< D.Fst, lazy begin fst (Lazy.force v0) end)
    | _ -> invalid_arg "Evaluation.fst"

  and snd : D.t -> D.t =
    function
    | D.Pair (_, v1) -> v1
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Snd)
    | D.Unfold (hd, frms, v0) ->
      D.Unfold (hd, frms #< D.Snd, lazy begin snd (Lazy.force v0) end)
    | _ -> invalid_arg "Evaluation.snd"

  and eval_ulvl =
    let module M = Mugenjou.Syntax in
    function
    | M.Top -> D.ULvl.top
    | M.Shifted (ulvl, s) -> D.ULvl.shifted (eval ulvl) s

  and eval : S.t -> D.t =
    function
    | S.Var idx -> Lazy.force (of_idx idx)
    | S.Global p -> of_global p
    | S.Pi (base, (* binding *) fam) -> D.Pi (eval base, make_clo fam)
    | S.Lambda (* binding *) body -> D.Lambda (make_clo body)
    | S.App (tm0, tm1) -> app (eval tm0) (eval tm1)
    | S.Sigma (base, (* binding *) fam) -> D.Sigma (eval base, make_clo fam)
    | S.Pair (t0, t1) -> D.Pair (eval t0, eval t1)
    | S.Fst t -> fst (eval t)
    | S.Snd t -> snd (eval t)
    | S.Univ t -> D.Univ (eval t)
    | S.ULvl l -> eval_ulvl l
end

type locals = Internal.locals
type resolve = Internal.resolve
type env = Internal.env = { locals : locals; resolve : resolve }

let app = Internal.app
let fst = Internal.fst
let snd = Internal.snd

let inst_clo = Internal.inst_clo
let inst_clo' = Internal.inst_clo'
let eval ~locals ~resolve tm = Internal.Eff.run ~env:{locals; resolve} @@ fun () -> Internal.eval tm
