open Algaeff.StdlibShim

module Syntax = Syntax

exception IllTyped = Checker.IllTyped
exception NotInScope = Scope.NotInScope

type resolve_data = Checker.resolve_data
type _ Effect.t +=
  | ResolveUnit = Scope.ResolveUnit
  | UnusedImports = Scope.UnusedImports

let execute = Execution.execute
