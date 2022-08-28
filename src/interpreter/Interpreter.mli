module Syntax : module type of Syntax

type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm : Syntax.t}
  | IllTyped of {tm : Syntax.t; tp : NbE.Domain.t}
  | Conversion of NbE.Domain.t * NbE.Domain.t

val execute : Syntax.prog -> (unit, error) result

type unused_info =
  | Imported of Bantorra.Manager.path Asai.Span.located
  | Local of Yuujinchou.Trie.path Asai.Span.located

module type Handler = UnitEffect.Handler
module Perform : Handler
module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
