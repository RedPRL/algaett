type cell = {tm : NbE.Domain.t; tp : NbE.Domain.t}
exception Error of Errors.t

val bind : name:Syntax.name option -> tp:NbE.Domain.t -> (NbE.Domain.t -> 'a) -> 'a

val trap : (unit -> 'a) -> ('a, Errors.t) Result.t

val blessed_ulvl : unit -> NbE.Domain.t

val resolve : Syntax.name -> ResolveData.t
val resolve_local : Syntax.name -> (cell * unit) option

(** invariant: the return values must be effect-less *)
val eval : NbE.Syntax.t -> NbE.Domain.t

val lazy_eval : NbE.Syntax.t -> NbE.Domain.t Lazy.t
val equate : NbE.Domain.t -> [ `EQ | `GE | `LE ] -> NbE.Domain.t -> unit

val quote : NbE.Domain.t -> NbE.Syntax.t

val not_inferable : tm:Syntax.t -> 'a
val ill_typed : tm:Syntax.t -> tp:NbE.Domain.t -> 'a

val with_top_env : (unit -> 'a) -> 'a

type handler = { resolve : Yuujinchou.Trie.path -> ResolveData.t }
val run : (unit -> 'a) -> handler -> 'a
val perform : handler
