open Tactic

val sigma : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
val pair : cfst:check -> csnd:check -> check
val fst : itm:infer -> infer
val snd : itm:infer -> infer
