exception Error of Errors.t

val bind : name:Yuujinchou.Trie.path option -> tp:NbE.Domain.t -> (NbE.Domain.cell -> 'a) -> 'a

val trap : (unit -> 'a) -> ('a, Errors.t) Result.t

val blessed_ulvl : unit -> NbE.Domain.t

val resolve : Yuujinchou.Trie.path -> ResolveData.t
val resolve_local : Yuujinchou.Trie.path -> (NbE.Domain.cell * unit) option

(** invariant: the return values must be effect-less *)
val eval : NbE.Syntax.t -> NbE.Domain.t

val lazy_eval : NbE.Syntax.t -> NbE.Domain.t Lazy.t
val equate : NbE.Domain.t -> [ `EQ | `GE | `LE ] -> NbE.Domain.t -> unit

val quote : NbE.Domain.t -> NbE.Syntax.t

val not_convertible : NbE.Domain.t -> NbE.Domain.t -> 'a

val with_top_env : (unit -> 'a) -> 'a

module type Handler =
sig
  val resolve : Yuujinchou.Trie.path -> ResolveData.t
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler