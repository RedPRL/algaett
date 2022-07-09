open RuleKit

let pi ~name ~cbase ~cfam : T.check =
  Quantifier.quantifier ~name ~cbase ~cfam S.pi

let vir_pi ~name ~cbase ~cfam : T.check =
  Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

let lam ~name ~cbnd : T.check =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Pi (base, fam) | D.VirPi (base, fam) ->
    Eff.bind ~name ~tp:base @@ fun arg ->
    let fib = NbE.inst_clo' fam @@ arg.D.tm in
    S.lam @@ T.Check.run {tp = fib; lhs = LHS.app goal.lhs arg} @@ cbnd arg
  | _ ->
    invalid_arg "lam"

let app ~itm ~ctm : T.infer =
  T.Infer.rule @@ fun _ ->
  let fn, fn_tp = T.Infer.run {lhs = LHS.unknown} itm in
  match NbE.force_all fn_tp with
  | D.Pi (base, fam) | D.VirPi (base, fam) ->
    let arg = T.Check.run {tp = base; lhs = LHS.unknown} ctm in
    let fib = NbE.inst_clo fam @@ Eff.lazy_eval arg in
    S.app fn arg, fib
  | _ ->
    invalid_arg "app"
