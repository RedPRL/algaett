module Syntax : module type of Syntax
module Domain : module type of Domain

val quote_con : Domain.t -> Syntax.t
val quote_cut : Domain.cut -> Syntax.t
val eval : Syntax.t -> Domain.t
val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t
val inst_clo : Domain.closure -> arg:Domain.t -> Domain.t

val run : locals:Domain.t Bwd.bwd
  -> resolve:(Yuujinchou.Trie.path -> [`Unfolded of Domain.t | `Folded | `NotFound])
  -> (unit -> 'a) -> 'a
