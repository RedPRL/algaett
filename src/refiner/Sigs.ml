module type InferPublic =
sig
  type t
  type goal = {lhs : NbE.LHS.t}
  type result = NbE.Syntax.t * NbE.Domain.t
  val run : goal -> t -> result
  val locate : loc:Asai.Span.t option -> t -> t

end

module type CheckPublic =
sig
  type t
  type infer

  type goal = {tp : NbE.Domain.t; lhs : NbE.LHS.t}
  type result = NbE.Syntax.t

  val run : goal -> t -> result

  val peek : (goal -> t) -> t
  val orelse : t -> (exn -> t) -> t
  val infer : infer -> t

  val locate : loc:Asai.Span.t option -> t -> t
end

module type ShiftPublic =
sig
  type t
  val run : t -> NbE.Domain.t
end

module type TacticPublic =
sig
  type infer
  type shift
  type check
  type hyp = NbE.Domain.cell

  type 'a binder = hyp -> 'a

  module Infer : InferPublic with type t = infer
  module Check : CheckPublic with type t = check and type infer := Infer.t
  module Shift : ShiftPublic with type t = shift
end
