open Algaeff.StdlibShim

module Syntax : module type of Syntax

exception NotInScope

type resolve_data = Checker.resolve_data

type _ Effect.t +=
  | ResolveUnit : Bantorra.Manager.unitpath -> Checker.resolve_data Yuujinchou.Trie.t Effect.t
  | UnusedImports : Bantorra.Manager.unitpath list -> unit Effect.t

val execute : Syntax.prog -> unit
