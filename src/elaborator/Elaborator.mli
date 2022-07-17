module Syntax : module type of Syntax
module Eff : module type of Eff

val infer_top : NbE.LHS.t -> Syntax.t -> NbE.Syntax.t * NbE.Domain.t
val check_tp_top : NbE.LHS.t -> Syntax.t -> NbE.Syntax.t
val check_top : NbE.LHS.t -> Syntax.t -> tp:NbE.Domain.t -> NbE.Syntax.t
