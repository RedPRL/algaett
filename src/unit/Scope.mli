exception NotInScope

val run : ?prefix:Yuujinchou.Trie.bwd_path -> (unit -> 'a) -> 'a
val include_singleton : (Yuujinchou.Trie.path * Decl.data) -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
