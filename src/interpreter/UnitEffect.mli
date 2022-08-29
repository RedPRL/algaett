val include_singleton : ?loc:Asai.Span.t -> (Yuujinchou.Trie.path * Refiner.ResolveData.t) -> unit
val import : ?loc:Asai.Span.t -> Bantorra.Manager.path -> Syntax.modifier -> unit
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
