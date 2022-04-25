exception NotInScope

val include_singleton : (Yuujinchou.Trie.path * Decl.data) -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
val run : (unit -> 'a) -> 'a
