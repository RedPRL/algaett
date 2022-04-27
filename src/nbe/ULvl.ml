module P = struct
  module Shift = Mugenjou.Shift.Gapped
  type var = int
  let equal_var = Int.equal
end

include Mugenjou.Builder.Free.Make (P)
include Mugenjou.Theory.Make (P)

let rec of_con =
  function
  | Domain.Cut (Lvl i, Emp) -> var i
  | Domain.ULvl endo -> of_endo endo
  | Domain.Unfold (_, _, v) -> of_con (Lazy.force v)
  | _ -> invalid_arg "of_con"

and of_endo =
  let module M = Mugenjou.Syntax in
  function
  | M.Shifted (l, s) -> shifted (of_con l) s
  | M.Top -> top
