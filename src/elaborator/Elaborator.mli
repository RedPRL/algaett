module Syntax : module type of Syntax
module Errors : module type of Errors

val infer_top : NbE.LHS.t -> Syntax.t -> (NbE.Syntax.t * NbE.Domain.t, Errors.t) result
val check_tp_top : NbE.LHS.t -> Syntax.t -> (NbE.Syntax.t, Errors.t) result
val check_top : NbE.LHS.t -> Syntax.t -> tp:NbE.Domain.t -> (NbE.Syntax.t, Errors.t) result

module type Handler =
sig
  include Refiner.Eff.Handler
  val unleash : Syntax.bound_name -> Refiner.ResolveData.t -> unit
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler
