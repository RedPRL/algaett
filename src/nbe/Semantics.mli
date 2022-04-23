val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t

val inst_clo : Domain.closure -> arg:Domain.t -> Domain.t
val eval : env:Domain.env -> Syntax.t -> Domain.t
