open RuleKit

let level (lvl : int) : T.infer =
  T.Infer.rule @@ fun _ ->
  let cell = Eff.resolve_level lvl in
  match cell with
  | None ->
    invalid_arg "level"
  | Some {tm; tp} ->
    Eff.quote tm, tp

let local_var (cell : D.cell) : T.infer =
  T.Infer.rule @@ fun _ ->
  Eff.quote cell.tm, cell.tp

let global_var path shift : T.infer =
  T.Infer.rule @@ fun _ ->
  let ulvl = T.Shift.run shift in
  let tm, tp =
    match Eff.resolve path with
    | Some ResolveData.Axiom {tp} -> S.axiom path, tp
    | Some ResolveData.Def {tp; tm} -> S.def path tm, tp
    | None -> Error.Logger.fatalf ?loc:(Eff.loc ()) ~code:NotInScope "The variable '%a' is not in scope" S.dump_name path

  in
  S.app tm (Eff.quote ulvl), NbE.app_ulvl ~tp ~ulvl

let ann ~ctp ~ctm : T.infer =
  T.Infer.rule @@ fun goal ->
  let tp = Eff.eval @@ T.Check.run {tp = D.univ_top; lhs = LHS.unknown} ctp in
  T.Check.run {tp; lhs = goal.lhs} ctm, tp
