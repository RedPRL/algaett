open RuleKit

let sigma ~name ~cbase ~cfam : T.check =
  Quantifier.quantifier ~name ~cbase ~cfam S.sigma

let pair ~cfst ~csnd : T.check =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Sigma (base, fam) ->
    let tm1 = T.Check.run {tp = base; lhs = LHS.fst goal.lhs} cfst in
    let tp2 = NbE.inst_clo fam @@ Eff.lazy_eval tm1 in
    let tm2 = T.Check.run {tp = tp2; lhs = LHS.snd goal.lhs} csnd in
    S.pair tm1 tm2
  | _ ->
    invalid_arg "pair"

let fst ~itm : T.infer =
  T.Infer.rule @@ fun _ ->
  let tm, tp = T.Infer.run {lhs = LHS.unknown} itm in
  match NbE.force_all tp with
  | D.Sigma (base, _) ->
    S.fst tm, base
  | _ ->
    invalid_arg "fst"

let snd ~itm : T.infer =
  T.Infer.rule @@ fun _ ->
  let tm, tp = T.Infer.run {lhs = LHS.unknown} itm in
  match NbE.force_all tp with
  | D.Sigma (_, fam) ->
    let tp = NbE.inst_clo fam @@ Eff.lazy_eval @@ S.fst tm in
    S.snd tm, tp
  | _ ->
    invalid_arg "snd"
