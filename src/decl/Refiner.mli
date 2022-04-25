open Algaeff.StdlibShim

type data =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm: NbE.Domain.t; tp: NbE.Domain.t}
type _ Effect.t += Resolve : Yuujinchou.Trie.path -> data Effect.t

exception IllTyped
val infer_top : Syntax.t -> NbE.Syntax.t * NbE.Domain.t
val check_top : Syntax.t -> tp:NbE.Domain.t -> NbE.Syntax.t
