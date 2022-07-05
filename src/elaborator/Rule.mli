type infer
type shift
type check
type hyp = RefineEffect.cell

type 'a binder = hyp -> 'a

module Infer :
sig
  type goal = {lhs : LHS.t}
  type result = NbE.Syntax.t * NbE.Domain.t

  type t = infer

  val rule : (goal -> result) -> t
  val run : goal -> t -> result
end

module Check :
sig
  type goal = {tp : NbE.Domain.t; lhs : LHS.t}
  type result = NbE.Syntax.t

  type t = check
  val rule : (goal -> result) -> t
  val run : goal -> t -> result

  val peek : (goal -> t) -> t
  val orelse : t -> (Errors.t -> t) -> t
  val infer : infer -> t
  val forcing : t -> t
end

module Shift :
sig
  type t = shift
  val rule : (unit -> NbE.Domain.t) -> t
  val run : t -> NbE.Domain.t
end
