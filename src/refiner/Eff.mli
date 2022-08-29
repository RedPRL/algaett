val bind : name:Yuujinchou.Trie.path option -> tp:NbE.Domain.t -> (NbE.Domain.cell -> 'a) -> 'a

val blessed_ulvl : unit -> NbE.Domain.t

val locate : loc:Asai.Span.t option -> (unit -> 'a) -> 'a

val loc : unit -> Asai.Span.t option

module type Handler =
sig
  val resolve : Yuujinchou.Trie.path Asai.Span.located -> ResolveData.t
end

module Perform : Handler
include module type of Perform

val resolve_local : Yuujinchou.Trie.path -> (NbE.Domain.cell * unit) option (* TODO: what is the unit for? *)
val resolve_level : int -> NbE.Domain.cell option

(** invariant: the return values must be effect-less *)
val eval : NbE.Syntax.t -> NbE.Domain.t

val lazy_eval : NbE.Syntax.t -> NbE.Domain.t SyncLazy.t
val equate : NbE.Domain.t -> [ `EQ | `GE | `LE ] -> NbE.Domain.t -> unit

val quote : NbE.Domain.t -> NbE.Syntax.t

val not_convertible : NbE.Domain.t -> NbE.Domain.t -> 'a

val with_top_env : (unit -> 'a) -> 'a

module Generalize :
sig
  type bnd = VirType of NbE.Syntax.t | Type of NbE.Syntax.t
  val quote_ctx : unit -> bnd list
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
