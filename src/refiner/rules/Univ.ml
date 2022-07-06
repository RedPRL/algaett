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
      Format.eprintf "@[<2>Universe@ level@ %a@ is@ not@ smaller@ than@ %a@]@."
        pp_lvl (UL.of_con vsmall)
        pp_lvl (UL.of_con large);
      invalid_arg "univ"
    end
  | _ ->
    invalid_arg "univ"
