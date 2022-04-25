module Syntax : module type of Syntax

exception NotInScope

val execute : Syntax.prog -> unit
