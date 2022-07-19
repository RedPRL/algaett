module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : Asai.Span.t -> Syntax.bound_name -> Refiner.ResolveData.t -> Syntax.name
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler
include module type of Perform
