module Syntax : module type of Syntax

type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}

val execute : Syntax.prog -> (unit, error) result

type unused_info =
  | Imported of Bantorra.Manager.path Elaborator.Syntax.node
  | Local of Yuujinchou.Trie.path Elaborator.Syntax.node

type handler =
  { load : Bantorra.Manager.path -> Elaborator.ResolveData.t Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : unused_info -> unit }

val run : (unit -> 'a) -> handler -> 'a
val perform : handler
