module Syntax : module type of Syntax
module Domain : module type of Domain

val quote_con : Domain.t -> Syntax.t
val quote_cut : Domain.cut -> Syntax.t
val eval : Syntax.t -> Domain.t
val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t
val inst_clo : Domain.closure -> arg:Domain.t Lazy.t -> Domain.t

val run : locals:Domain.env
  -> resolve:(Yuujinchou.Trie.path -> Domain.t option)
  -> (unit -> 'a) -> 'a
