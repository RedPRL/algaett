type info =
  | Imported of Bantorra.Manager.path Checker.Syntax.node
  | Local of Yuujinchou.Trie.path Checker.Syntax.node

type id
val compare_id : id -> id -> int

val new_ : info -> id
val use : id -> unit

type handler = { warn_unused : info -> unit }

val run : (unit -> 'a) -> handler -> 'a
val perform : handler
