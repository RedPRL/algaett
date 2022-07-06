module Syntax = Syntax

type error = UnitEffect.error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
  | Conversion of NbE.Domain.t * NbE.Domain.t

let execute = Driver.execute

type unused_info = Used.info =
  | Imported of Bantorra.Manager.path Elaborator.Syntax.node
  | Local of Yuujinchou.Trie.path Elaborator.Syntax.node

type handler = UnitEffect.handler =
  { load : Bantorra.Manager.path -> Refiner.ResolveData.t Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : Used.info -> unit }

let run = UnitEffect.run

let perform = UnitEffect.perform
