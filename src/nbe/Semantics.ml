open Bwd
open BwdNotation

module S = Syntax
module D = Domain

module Internal =
struct
  type env =
    { locals : D.env;
      resolve : Yuujinchou.Trie.path -> [`Unfolded of Domain.t | `Folded | `NotFound] }
  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let make_clo body = D.Clo {body; env = (Eff.read()).locals}

  exception Unresolved of Yuujinchou.Trie.path
  let of_idx idx = BwdLabels.nth (Eff.read()).locals idx
  let of_global p =
    match (Eff.read()).resolve p with
    | `Unfolded tm -> tm
    | `Folded -> D.global p
    | `NotFound -> raise (Unresolved p)

  let rec inst_clo (D.Clo {body; env}) ~arg : D.t =
    let env = {(Eff.read()) with locals = env #< arg} in
    Eff.run ~env @@ fun () -> eval body

  and app v0 v1 =
    match v0 with
    | D.Lambda clo -> inst_clo clo ~arg:v1
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< (D.App v1))
    | _ -> invalid_arg "Evaluation.app"

  and fst : D.t -> D.t =
    function
    | D.Pair (t0, _) -> t0
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Fst)
    | _ -> invalid_arg "Evaluation.fst"

  and snd : D.t -> D.t =
    function
    | D.Cut (hd, frms) ->
      D.Cut (hd, frms #< D.Snd)
    | D.Pair (_, t1) -> t1
    | _ -> invalid_arg "Evaluation.snd"

  and eval_ulvl =
    let module M = Mugenjou.Syntax in
    function
    | M.Top -> D.ULvlBuilder.top
    | M.Shifted (ulvl, s) -> D.ULvlBuilder.shifted (eval ulvl) s

  and eval : S.t -> D.t =
    function
    | S.Var idx -> of_idx idx
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

type env = Internal.env =
  { locals : Domain.env;
    resolve : Yuujinchou.Trie.path -> [`Unfolded of Domain.t | `Folded | `NotFound] }

let app = Internal.app
let fst = Internal.fst
let snd = Internal.snd

let inst_clo = Internal.inst_clo
let eval ~env tm = Internal.Eff.run ~env @@ fun () -> Internal.eval tm
