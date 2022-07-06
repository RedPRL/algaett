module Syntax : module type of Syntax
module Errors : module type of Errors

val infer_top : NbE.LHS.t -> Syntax.t -> (NbE.Syntax.t * NbE.Domain.t, Errors.t) result
val check_tp_top : NbE.LHS.t -> Syntax.t -> (NbE.Syntax.t, Errors.t) result
val check_top : NbE.LHS.t -> Syntax.t -> tp:NbE.Domain.t -> (NbE.Syntax.t, Errors.t) result

type handler = Refiner.Eff.handler
val run : (unit -> 'a) -> handler -> 'a
val perform : handler
