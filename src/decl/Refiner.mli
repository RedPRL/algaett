(* effects: Scope *)
val infer_top : Syntax.t -> NbE.Syntax.t * NbE.Domain.t

(* effects: Scope *)
val check_top : Syntax.t -> tp:NbE.Domain.t -> NbE.Syntax.t
