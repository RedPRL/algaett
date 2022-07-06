open Tactic

val local_var : hyp -> infer
val global_var : Yuujinchou.Trie.path -> shift -> infer
val ann : ctp:check -> ctm:check -> infer
