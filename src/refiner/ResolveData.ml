type t =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm : NbE.Domain.t Lazy.t; tp : NbE.Domain.t}