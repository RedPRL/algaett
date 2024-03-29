module Syntax : module type of Syntax
module Domain : module type of Domain
module ULvl : module type of ULvl
module LHS : module type of LHS

val inst_clo : Domain.closure -> Domain.t SyncLazy.t -> Domain.t
val inst_clo' : Domain.closure -> Domain.t -> Domain.t
val app_ulvl : tp:Domain.t -> ulvl:Domain.t -> Domain.t

val eval : env:Domain.env -> Syntax.t -> Domain.t
val eval_top : Syntax.t -> Domain.t

val quote : size:int -> Domain.t -> Syntax.t

exception Unequal
val force_all : Domain.t -> Domain.t
val equate : size:int -> Domain.t -> [`LE | `EQ | `GE] -> Domain.t -> unit
