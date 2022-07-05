type infer
type shift
type check
type hyp = RefineEffect.cell

type 'a binder = hyp -> 'a

module Infer :
sig
  type t = infer
  val rule : (unit -> NbE.Syntax.t * NbE.Domain.t) -> t
  val run : t -> NbE.Syntax.t * NbE.Domain.t
end

module Check :
sig
  type t = check
  val rule : (tp:NbE.Domain.t -> NbE.Syntax.t) -> t
  val run : tp:NbE.Domain.t -> t -> NbE.Syntax.t
  val peek : (tp:NbE.Domain.t -> t) -> t
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
