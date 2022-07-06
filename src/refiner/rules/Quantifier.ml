open RuleKit

type rule =
  name:Yuujinchou.Trie.path option
  -> cbase:T.check
  -> cfam:T.check T.binder
  -> (S.t -> S.t -> S.t)
  -> T.check

let quantifier ~name ~cbase ~cfam syn : T.check =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Univ _ ->
    let base = T.Check.run {tp = goal.tp; lhs = LHS.unknown} cbase in
    let vbase = Eff.eval base in
    let fam =
      Eff.bind ~name ~tp:vbase @@ fun x ->
      T.Check.run {tp = goal.tp; lhs = LHS.unknown} @@ cfam x
    in
    syn base fam
  | _ ->
    invalid_arg "quantifier"

let vir_quantifier ~name ~cbase ~cfam syn : T.check =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Univ _ ->
    let base = T.Check.run {tp = D.VirUniv; lhs = LHS.unknown} cbase in
    let vbase = Eff.eval base in
    let fam =
      Eff.bind ~name ~tp:vbase @@ fun x ->
      T.Check.run {tp = goal.tp; lhs = LHS.unknown} @@ cfam x
    in
    syn base fam
  | _ ->
    invalid_arg "quantifier"
