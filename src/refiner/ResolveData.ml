type t =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm : NbE.Domain.t SyncLazy.t; tp : NbE.Domain.t}
