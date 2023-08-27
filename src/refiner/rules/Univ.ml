open RuleKit

let univ shift =
  T.Check.rule @@ fun goal ->
  match goal.tp with
  | D.Univ large ->
    let vsmall = T.Shift.run shift in
    if UL.lt (UL.of_con vsmall) (UL.of_con large)
    then S.univ (Eff.quote vsmall)
    else begin
      let pp_lvl = Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int in
      E.fatalf IllTyped
        "Universe level %a is not smaller than %a. This type is too large to fit into the universe."
        pp_lvl (UL.of_con vsmall)
        pp_lvl (UL.of_con large)
    end
  | tp ->
    Error.expected_connective_check `Univ S.dump (Eff.quote tp)
