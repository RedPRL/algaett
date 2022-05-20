type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
val reraise_checker : ('a, Checker.error) result -> 'a
val trap : (unit -> 'a) -> ('a, error) result

val include_singleton : (Yuujinchou.Trie.path * Checker.resolve_data) -> unit
val import : Bantorra.Manager.path Checker.Syntax.node -> Syntax.modifier -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
val get_export : unit -> Checker.resolve_data Yuujinchou.Trie.Untagged.t

type handler =
  { load : Bantorra.Manager.path -> Checker.resolve_data Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : Bantorra.Manager.path Syntax.node -> unit }
val run : (unit -> 'a) -> handler -> 'a
val reperform : handler
