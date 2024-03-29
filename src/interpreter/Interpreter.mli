module Syntax : module type of Syntax

val execute : Syntax.prog -> unit

type unused_info =
  | Imported of Bantorra.Manager.path Asai.Span.located
  | Local of Yuujinchou.Trie.path Asai.Span.located

module type Handler = UnitEffect.Handler
module Perform : Handler
module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
