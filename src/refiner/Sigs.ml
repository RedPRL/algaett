module type InferPublic =
sig
  type t
  type goal = {lhs : NbE.LHS.t}
  type result = NbE.Syntax.t * NbE.Domain.t
  val run : goal -> t -> result
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
  val forcing : t -> t
end

module type ShiftPublic =
sig
  type t
  val run : t -> NbE.Domain.t
end
