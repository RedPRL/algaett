open Tactic

val pi : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
val vir_pi : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
val lam : name:Yuujinchou.Trie.path option -> cbnd:check binder -> check
val app : itm:infer -> ctm:check -> infer
