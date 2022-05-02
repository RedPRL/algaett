open Algaeff.StdlibShim

module Syntax : module type of Syntax

type resolve_data =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm: NbE.Domain.t Lazy.t; tp: NbE.Domain.t}
type _ Effect.t += Resolve : Yuujinchou.Trie.path -> resolve_data Effect.t

exception NotInferable of {tm: Syntax.t}
exception IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
val infer_top : Syntax.t -> NbE.Syntax.t * NbE.Domain.t
val check_tp_top : Syntax.t -> NbE.Syntax.t
val check_top : Syntax.t -> tp:NbE.Domain.t -> NbE.Syntax.t
