open Bwd

module S = Syntax
module D = Domain
module Sem = Semantics

module Internal =
struct
  module Eff = Algaeff.Reader.Make (struct type env = int end)

  let bind tm =
    let arg = D.lvl @@ Eff.read() in
    Eff.scope (fun lvl -> lvl + 1) @@ fun () ->
    tm arg

  let rec quote_con tm =
    match tm with
    | D.Cut cut -> quote_cut cut
    | D.Lambda clo ->
      S.lam @@ quote_clo clo
    | D.Pair (t1, t2) ->
      S.pair (quote_con t1) (quote_con t2)
    | D.Pi (base, fam) ->
      S.pi (quote_con base) (quote_clo fam)
    | D.Sigma (base, fam) ->
      S.sigma (quote_con base) (quote_clo fam)
    | D.Univ t -> S.univ (quote_con t)
    | D.ULvl l -> S.ulvl (quote_ulvl l)

  and quote_ulvl =
    let module M = Mugenjou.Syntax in
    function
    | M.Top -> M.Top
    | M.Shifted (ulvl, s) -> M.Shifted (quote_con ulvl, s)

  and quote_clo clo =
    bind @@ fun arg -> quote_con (Sem.inst_clo clo ~arg)

  and quote_cut (hd, frms) =
    BwdLabels.fold_left ~f:quote_frm ~init:(quote_hd hd) frms

  and quote_hd =
    function
    | D.Lvl l -> S.var @@ Eff.read() - l - 1
    | D.Global p -> S.Global p

  and quote_frm hd : D.frame -> S.t =
    function
    | D.App t -> S.app hd (quote_con t)
    | D.Fst -> S.fst hd
    | D.Snd -> S.snd hd
end

let con ~size tm = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_con tm
let cut ~size c = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_cut c
