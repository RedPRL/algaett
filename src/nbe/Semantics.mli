type locals = Domain.env

val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t

val inst_clo : Domain.closure -> Domain.t Lazy.t -> Domain.t
val inst_clo' : Domain.closure -> Domain.t -> Domain.t
val eval : locals:locals -> Syntax.t -> Domain.t
