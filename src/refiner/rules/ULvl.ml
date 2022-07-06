open RuleKit

let shifted t i =
  T.Shift.rule @@ fun () ->
  D.ULvl.shifted (T.Shift.run t) (NbE.ULvl.Shift.of_int i)

let base =
  T.Shift.rule Eff.blessed_ulvl
