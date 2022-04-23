open Bwd
open BwdNotation

module S = Syntax
module D = Domain

module Internal =
struct
  module Eff = Algaeff.Reader.Make (struct type env = D.t bwd end)

  let make_clo body = D.Clo {body; env = Eff.read()}

  let of_idx idx = BwdLabels.nth (Eff.read ()) idx

  let rec inst_clo (D.Clo {body; env}) ~arg : D.t =
    Eff.run ~env:(env #< arg) @@ fun () -> eval body

  and app v0 v1 =
    match v0 with
    | D.Lambda clo -> inst_clo clo ~arg:v1
    | D.Cut {tp = D.Pi (base, fam); cut = hd, frms} ->
      D.Cut {tp = inst_clo fam ~arg:v1; cut = hd, frms #< (D.App {tp = base; tm = v1})}
    | _ -> invalid_arg "Evaluation.app"

  and fst : D.t -> D.t =
    function
    | D.Pair (t0, _) -> t0
    | D.Cut {tp = D.Sigma (base, _); cut = hd, frms} ->
      D.Cut {tp = base; cut = (hd, frms #< D.Fst)}
    | _ -> invalid_arg "Evaluation.fst"

  and snd : D.t -> D.t =
    function
    | D.Cut {tp = D.Sigma (_, fam); cut = hd, frms} as v ->
      D.Cut {tp = inst_clo fam ~arg:(fst v); cut = hd, frms #< D.Snd}
    | D.Pair (_, t1) -> t1
    | _ -> invalid_arg "Evaluation.snd"

  and eval : S.t -> D.t =
    function
    | S.Var idx -> of_idx idx
    | S.Pi (base, (* binding *) fam) -> D.Pi (eval base, make_clo fam)
    | S.Lambda (* binding *) body -> D.Lambda (make_clo body)
    | S.App (tm0, tm1) -> app (eval tm0) (eval tm1)
    | S.Sigma (base, (* binding *) fam) -> D.Sigma (eval base, make_clo fam)
    | S.Pair (t0, t1) -> D.Pair (eval t0, eval t1)
    | S.Fst t -> fst (eval t)
    | S.Snd t -> snd (eval t)
    | S.Univ -> D.Univ
end

let app = Internal.app
let fst = Internal.fst
let snd = Internal.snd

let inst_clo = Internal.inst_clo
let eval ~env tm = Internal.Eff.run ~env @@ fun () -> Internal.eval tm
