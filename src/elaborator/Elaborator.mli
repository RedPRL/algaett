module Syntax : module type of Syntax
module Errors : module type of Errors
module ResolveData : module type of ResolveData
module LHS : module type of LHS

val infer_top : LHS.t -> Syntax.t -> (NbE.Syntax.t * NbE.Domain.t, Errors.t) result
val check_tp_top : LHS.t -> Syntax.t -> (NbE.Syntax.t, Errors.t) result
val check_top : LHS.t -> Syntax.t -> tp:NbE.Domain.t -> (NbE.Syntax.t, Errors.t) result

type handler = RefineEffect.handler
val run : (unit -> 'a) -> handler -> 'a
val perform : handler
