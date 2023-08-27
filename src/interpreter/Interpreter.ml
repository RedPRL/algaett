module Syntax = Syntax

let execute = Driver.execute

type unused_info = Used.info =
  | Imported of Bantorra.Manager.path Asai.Span.located
  | Local of Yuujinchou.Trie.path Asai.Span.located

module type Handler = UnitEffect.Handler
module Run = UnitEffect.Run
module Perform = UnitEffect.Perform
