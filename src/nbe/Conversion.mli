val force_all : Domain.t -> Domain.t
val equate : size:int -> Domain.t -> [`LE | `EQ | `GE] -> Domain.t -> unit
