open RuleKit

let univ shift =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Univ large ->
    let vsmall = T.Shift.run shift in
    if UL.(<) (UL.of_con vsmall) (UL.of_con large)
    then S.univ (Eff.quote vsmall)
    else begin
      let pp_lvl = Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int in
      let message = Format.asprintf "@[<2>Universe@ level@ %a@ is@ not@ smaller@ than@ %a@]@."
        pp_lvl (UL.of_con vsmall)
        pp_lvl (UL.of_con large)
      in
      let cause = "This type is too large to fit in the universe it is being checked against" in
      E.Doctor.build ~code:IllTyped ~cause ~message |> E.Doctor.fatal
    end
  | tp ->
    E.Doctor.expected_connective_check `Univ S.dump (Eff.quote tp)
