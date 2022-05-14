exception NotInScope
type _ Effect.t +=
  | ResolveUnit : Bantorra.Manager.path -> Checker.resolve_data Yuujinchou.Trie.t Effect.t
  | UnusedImports : Bantorra.Manager.path list -> unit Effect.t

type empty = |
type modifier = empty Yuujinchou.Language.modifier

val include_singleton : (Yuujinchou.Trie.path * Checker.resolve_data) -> unit
val import : Bantorra.Manager.path -> modifier -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
val get_export : unit -> Checker.resolve_data Yuujinchou.Trie.t
val run : (unit -> 'a) -> 'a
