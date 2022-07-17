type info =
  | Imported of Bantorra.Manager.path Asai.Span.located
  | Local of Yuujinchou.Trie.path Asai.Span.located

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
