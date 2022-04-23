type env =
  { locals : Domain.env;
    resolve : Yuujinchou.Trie.path -> [`Unfolded of Domain.t | `Folded | `NotFound] }

val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t

val inst_clo : Domain.closure -> arg:Domain.t -> Domain.t
val eval : env:env -> Syntax.t -> Domain.t
