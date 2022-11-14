module Shift = Data.ULvlShift

include
  Mugen.Semantics.Free.Make
    (struct
      module Shift = Shift
      type var = int
      let equal_var = Int.equal
    end)

let rec of_con =
  function
  | Domain.Cut (Lvl i, Emp) -> var i
  | Domain.ULvl endo -> of_endo endo
  | Domain.Unfold (_, _, v) -> of_con (SyncLazy.force v)
  | _ -> invalid_arg "of_con"

and of_endo =
  let module M = Mugen.Syntax in
  function
  | M.Shifted (l, s) -> shifted (of_con l) s
  | M.Top -> top
