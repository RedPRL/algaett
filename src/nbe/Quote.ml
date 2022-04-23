open Bwd

module S = Syntax
module D = Domain
module Sem = Semantics

module Internal =
struct
  module Eff = Algaeff.Reader.Make (struct type env = int end)

  let bind ~tp tm =
    let arg = D.lvl ~tp @@ Eff.read () in
    Eff.scope (fun lvl -> lvl + 1) @@ fun () ->
    tm arg

  let rec quote_con ~tp tm =
    match tp, tm with
    | _, D.Cut {tp = _; cut = cut} ->
      quote_cut cut
    | D.Pi (base, fam), v ->
      S.lam @@ bind ~tp:base @@ fun arg -> quote_con ~tp:(Sem.inst_clo fam ~arg) (Sem.app v arg)
    | D.Sigma (base, fam), v ->
      let fst = Sem.fst v in
      (* XXX one should avoid computing [Eval.inst_clo fam fst] twice (once here, once in snd) *)
      S.pair (quote_con ~tp:base fst) (quote_con ~tp:(Sem.inst_clo fam ~arg:fst) (Sem.snd v))
    | D.Univ, D.Pi (base, fam) ->
      S.pi (quote_con ~tp base) (quote_fam base tp fam)
    | D.Univ, D.Sigma (base, fam) ->
      S.sigma (quote_con ~tp base) (quote_fam base tp fam)
    | _ -> invalid_arg "quote_con"

  and quote_fam base univ clo =
    bind ~tp:base @@ fun arg ->
    quote_con ~tp:univ (Sem.inst_clo clo ~arg)

  and quote_cut (hd, frms) =
    BwdLabels.fold_left ~f:quote_frm ~init:(quote_hd hd) frms

  and quote_hd =
    function
    | D.Lvl l -> S.var @@ Eff.read() - l - 1

  and quote_frm hd : D.frame -> S.t =
    function
    | D.App {tp; tm} -> S.app hd (quote_con ~tp tm)
    | D.Fst -> S.fst hd
    | D.Snd -> S.snd hd
end

let con ~size ~tp tm = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_con ~tp tm
let cut ~size c = Internal.Eff.run ~env:size @@ fun () -> Internal.quote_cut c
