type env =
  { locals : Domain.env;
    resolve : Yuujinchou.Trie.path -> Domain.t option }

val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t

val inst_clo : Domain.closure -> arg:Domain.t Lazy.t -> Domain.t
val eval : env:env -> Syntax.t -> Domain.t
