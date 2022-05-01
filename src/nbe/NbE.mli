module Syntax : module type of Syntax
module Domain : module type of Domain
module ULvl : module type of ULvl

val inst_clo : Domain.closure -> Domain.t Lazy.t -> Domain.t
val inst_clo' : Domain.closure -> Domain.t -> Domain.t
val eval : env:Domain.env -> Syntax.t -> Domain.t
val eval_top : Syntax.t -> Domain.t

val quote : size:int -> Domain.t -> Syntax.t

exception Unequal
val force_all : Domain.t -> Domain.t
val equate : size:int -> Domain.t -> [`LE | `EQ | `GE] -> Domain.t -> unit
