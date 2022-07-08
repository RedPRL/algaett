module Syntax = Syntax

type error = UnitEffect.error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm : Syntax.t}
  | IllTyped of {tm : Syntax.t; tp : NbE.Domain.t}
  | Conversion of NbE.Domain.t * NbE.Domain.t

let execute = Driver.execute

type unused_info = Used.info =
  | Imported of Bantorra.Manager.path Elaborator.Syntax.node
  | Local of Yuujinchou.Trie.path Elaborator.Syntax.node

module type Handler = UnitEffect.Handler
module Run = UnitEffect.Run
module Perform = UnitEffect.Perform
