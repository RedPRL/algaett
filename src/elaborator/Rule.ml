module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl
module LHS = NbE.LHS

type hyp = D.cell
type 'a binder = hyp -> 'a

module Infer =
struct
  type goal = {lhs : LHS.t}
  type result = S.t * D.t

  type t = goal -> result
  let rule t = t
  let run goal t = t goal
end

type infer = Infer.t

module Check =
struct
  type goal = {tp : D.t; lhs : LHS.t}
  type result = S.t

  type t = goal -> result

  let rule t = t
  let run goal t = t goal
  let peek t goal = t goal goal

  let forcing (t : t) : t =
    fun goal ->
    t {goal with tp = NbE.force_all goal.tp}

  let infer (inf : infer) : t =
    fun goal ->
    let tm', tp' = Infer.run Infer.{lhs = goal.lhs} inf in
    try RefineEffect.equate tp' `LE goal.tp; tm' with
    | NbE.Unequal -> RefineEffect.not_convertible goal.tp tp'

  let orelse t k : t =
    fun goal ->
    try t goal with
    | RefineEffect.Error err ->
      k err goal

end

type check = Check.t

module Shift =
struct
  type t = unit -> D.t
  let rule t = t
  let run t = t ()
end

type shift = Shift.t
