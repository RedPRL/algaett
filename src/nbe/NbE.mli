module Syntax : module type of Syntax
module Domain : module type of Domain

val quote_con : size:int -> Domain.t -> Syntax.t
val quote_cut : size:int -> Domain.cut -> Syntax.t
val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t
val inst_clo : Domain.closure -> arg:Domain.t Lazy.t -> Domain.t
val inst_clo' : Domain.closure -> arg:Domain.t -> Domain.t
val eval : locals:Domain.env -> resolve:(Yuujinchou.Trie.path -> Domain.t option) -> Syntax.t -> Domain.t
