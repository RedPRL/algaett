open RuleKit

let local_var (cell : D.cell) =
  T.Infer.rule @@ fun _ ->
  Eff.quote cell.tm, cell.tp

let global_var path shift =
  T.Infer.rule @@ fun _ ->
  let ulvl = T.Shift.run shift in
  let tm, tp =
    match Eff.resolve path with
    | ResolveData.Axiom {tp} -> S.axiom path, tp
    | ResolveData.Def {tp; tm} -> S.def path tm, tp
  in
  S.app tm (Eff.quote ulvl), NbE.app_ulvl ~tp ~ulvl

let ann ~ctp ~ctm : T.infer =
  T.Infer.rule @@ fun goal ->
  let tp = Eff.eval @@ T.Check.run {tp = D.univ_top; lhs = LHS.unknown} ctp in
  T.Check.run {tp; lhs = goal.lhs} ctm, tp
