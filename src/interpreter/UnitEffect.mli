type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}

val reraise_elaborator : ('a, Elaborator.Errors.t) result -> 'a
val trap : (unit -> 'a) -> ('a, error) result

val include_singleton : ?loc:Elaborator.Syntax.span -> (Yuujinchou.Trie.path * Elaborator.ResolveData.t) -> unit
val import : ?loc:Elaborator.Syntax.span -> Bantorra.Manager.path -> Syntax.modifier -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
val get_export : unit -> Elaborator.ResolveData.t Yuujinchou.Trie.Untagged.t

type handler =
  { load : Bantorra.Manager.path -> Elaborator.ResolveData.t Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : Used.info -> unit }
val run : (unit -> 'a) -> handler -> 'a
val perform : handler
