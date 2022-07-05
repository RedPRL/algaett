module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type hyp = RefineEffect.cell
type 'a binder = hyp -> 'a

module Hyp = RefineEffect.Cell

module Infer : sig
  type t
  val rule : (unit -> NbE.Syntax.t * NbE.Domain.t) -> t
  val run : t -> NbE.Syntax.t * NbE.Domain.t
end =
struct
  type t = unit -> S.t * D.t
  let rule t = t
  let run t = t ()
end

type infer = Infer.t

module Check : sig
  type t

  val rule : (tp:NbE.Domain.t -> NbE.Syntax.t) -> t

  val run : tp:NbE.Domain.t -> t -> NbE.Syntax.t
  val peek : (tp:NbE.Domain.t -> t) -> t
  val orelse : t -> (Errors.t -> t) -> t
  val infer : infer -> t
  val forcing : t -> t
end =
struct
  type t = tp:D.t -> S.t

  let rule t = t
  let run ~tp t = t ~tp
  let peek t ~tp = t ~tp ~tp

  let forcing (t : t) : t = fun ~tp ->
    t ~tp:(NbE.force_all tp)

  let infer (inf : infer) : t = fun ~tp ->
    let tm', tp' = Infer.run inf in
    try RefineEffect.equate tp' `LE tp; tm' with
    | NbE.Unequal -> RefineEffect.not_convertible tp tp'

  let orelse t k : t = fun ~tp ->
    try t ~tp with
    | RefineEffect.Error err ->
      k err ~tp

end

type check = Check.t

module Shift =
struct
  type t = unit -> D.t
  let rule t = t
  let run t = t ()
end

type shift = Shift.t
