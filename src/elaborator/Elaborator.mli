val infer_top : Syntax.t -> (NbE.Syntax.t * NbE.Domain.t, Errors.t) result
val check_tp_top : Syntax.t -> (NbE.Syntax.t, Errors.t) result
val check_top : Syntax.t -> tp:NbE.Domain.t -> (NbE.Syntax.t, Errors.t) result

type handler = RefineEffect.handler
val run : (unit -> 'a) -> handler -> 'a
val perform : handler

module Syntax : module type of Syntax
module Errors : module type of Errors
module ResolveData : module type of ResolveData