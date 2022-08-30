open RuleKit

type rule =
  name:Yuujinchou.Trie.path option
  -> cbase:T.check
  -> cfam:T.check T.binder
  -> (S.t -> S.t -> S.t)
  -> T.check

let auxiliary ~name ~cbase ~cbase_sort ~cfam builder : T.check =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Univ _ ->
    let base = T.Check.run {tp = cbase_sort goal.tp; lhs = LHS.unknown} cbase in
    let vbase = Eff.eval base in
    let fam =
      Eff.bind ~name ~tp:vbase @@ fun x ->
      T.Check.run {tp = goal.tp; lhs = LHS.unknown} @@ cfam x
    in
    builder base fam
  | tp ->
    Error.Connective.check ?loc:(Eff.loc ()) `Univ S.dump (Eff.quote tp)


let quantifier ~name ~cbase ~cfam builder : T.check =
  auxiliary ~name ~cbase ~cbase_sort:(fun univ -> univ) ~cfam builder

let vir_quantifier ~name ~cbase ~cfam builder : T.check =
  auxiliary ~name ~cbase ~cbase_sort:(fun _ -> D.VirUniv) ~cfam builder
