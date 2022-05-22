module Syntax = Syntax

type error = UnitEffect.error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}

let execute = Driver.execute

type handler = UnitEffect.handler =
  { load : Bantorra.Manager.path -> Checker.resolve_data Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : Used.info -> unit }
let run = UnitEffect.run
let perform = UnitEffect.perform
