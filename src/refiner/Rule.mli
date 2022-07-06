type infer
type shift
type check
type hyp = NbE.Domain.cell

type 'a binder = hyp -> 'a

module Infer :
sig
  include Sigs.InferPublic with type t = infer
  val rule : (goal -> result) -> t
end

module Check :
sig
  include Sigs.CheckPublic with type t = check and type infer := Infer.t
  val rule : (goal -> result) -> t
end

module Shift :
sig
  include Sigs.ShiftPublic with type t = shift
  val rule : (unit -> NbE.Domain.t) -> t
end
