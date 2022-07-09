type info =
  | Imported of Bantorra.Manager.path Elaborator.Syntax.node
  | Local of Yuujinchou.Trie.path Elaborator.Syntax.node

type id
val compare_id : id -> id -> int

val new_ : info -> id
val use : id -> unit

module type Handler =
sig
  val warn_unused : info -> unit
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler
