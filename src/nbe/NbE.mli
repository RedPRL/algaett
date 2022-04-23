module Syntax : module type of Syntax
module Domain : module type of Domain

type cell = {tp : Domain.t; tm : Domain.t}

val quote_con : tp:Domain.t -> Domain.t -> Syntax.t
val quote_cut : Domain.cut -> Syntax.t
val eval : Syntax.t -> Domain.t
val app : Domain.t -> Domain.t -> Domain.t
val fst : Domain.t -> Domain.t
val snd : Domain.t -> Domain.t
val inst_clo : Domain.closure -> arg:Domain.t -> Domain.t

val run : env:cell Bwd.bwd -> (unit -> 'a) -> 'a
