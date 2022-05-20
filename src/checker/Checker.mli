module Syntax : module type of Syntax

type resolve_data =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm: NbE.Domain.t Lazy.t; tp: NbE.Domain.t}

type error =
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
val infer_top : Syntax.t -> (NbE.Syntax.t * NbE.Domain.t, error) result
val check_tp_top : Syntax.t -> (NbE.Syntax.t, error) result
val check_top : Syntax.t -> tp:NbE.Domain.t -> (NbE.Syntax.t, error) result

type handler = { resolve : Yuujinchou.Trie.path -> resolve_data }
val run : (unit -> 'a) -> handler -> 'a
val reperform : handler
