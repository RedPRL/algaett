type t

val unknown : t
val head : Yuujinchou.Trie.path -> t
val app : t -> Domain.cell -> t
val fst : t -> t
val snd : t -> t
