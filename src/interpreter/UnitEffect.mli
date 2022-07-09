type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
  | Conversion of NbE.Domain.t * NbE.Domain.t

val reraise_elaborator : ('a, Elaborator.Errors.t) result -> 'a
val trap : (unit -> 'a) -> ('a, error) result

val include_singleton : ?loc:Elaborator.Syntax.span -> (Yuujinchou.Trie.path * Refiner.ResolveData.t) -> unit
val import : ?loc:Elaborator.Syntax.span -> Bantorra.Manager.path -> Syntax.modifier -> unit
val section : Yuujinchou.Trie.path -> (unit -> 'a) -> 'a
val get_export : unit -> Refiner.ResolveData.t Yuujinchou.Trie.Untagged.t

module type Handler =
sig
  val load : Bantorra.Manager.path -> Refiner.ResolveData.t Yuujinchou.Trie.Untagged.t
  val preload : Bantorra.Manager.path -> unit
  val warn_unused : Used.info -> unit
end

module Run (H : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end

module Perform : Handler
