module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : Syntax.bound_name -> Refiner.ResolveData.t -> Syntax.name
end

module Handle (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler
include module type of Perform

exception Error of Errors.t

val not_inferable : tm:Syntax.t -> 'a
val ill_typed : tm:Syntax.t -> tp:NbE.Domain.t -> 'a
