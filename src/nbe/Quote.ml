open Bwd

module S = Syntax
module D = Domain
module Sem = Semantics

module Internal =
struct
  module Eff = Algaeff.Reader.Make (struct type env = int end)

  let bind f =
    let arg = D.lvl @@ Eff.read() in
    Eff.scope (fun size -> size + 1) @@ fun () ->
    f arg

  let rec quote_con =
    function
    | D.Cut cut -> quote_cut cut
    | D.Unfold unfold -> quote_unfold unfold
    | D.Lam clo ->
      S.lam @@ quote_clo clo
    | D.Pair (v1, v2) ->
      S.pair (quote_con v1) (quote_con v2)
    | D.Pi (base, fam) ->
      S.pi (quote_con base) (quote_clo fam)
    | D.Sigma (base, fam) ->
      S.sigma (quote_con base) (quote_clo fam)
    | D.Univ v -> S.univ (quote_con v)
    | D.VirPi (base, fam) ->
      S.vir_pi (quote_con base) (quote_clo fam)
    | D.TpULvl -> S.tp_ulvl
    | D.ULvl l -> S.ulvl (quote_ulvl l)
    | D.VirUniv -> S.vir_univ

  and quote_ulvl =
    let module M = Mugen.Syntax in
    function
    | M.Top -> M.Top
    | M.Shifted (ulvl, s) -> M.Shifted (quote_con ulvl, s)

  and quote_clo clo =
    bind @@ fun arg -> quote_con (Sem.inst_clo' clo arg)

  and quote_cut (hd, frms) =
    BwdLabels.fold_left ~f:quote_frm ~init:(quote_cut_hd hd) frms

  and quote_cut_hd =
    function
    | D.Lvl l -> S.var @@ Eff.read() - l - 1
    | D.Axiom p -> S.axiom p

  and quote_unfold (hd, frms, _) =
    BwdLabels.fold_left ~f:quote_frm ~init:(quote_unfold_hd hd) frms

  and quote_unfold_hd =
    function
    | D.Def (p, v) -> S.def p v

  and quote_frm hd : D.frame -> S.t =
    function
    | D.App t -> S.app hd (quote_con t)
    | D.Fst -> S.fst hd
    | D.Snd -> S.snd hd
end

let con ~size tm = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_con tm
let cut ~size c = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_cut c
