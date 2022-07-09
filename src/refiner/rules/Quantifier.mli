open Tactic

type rule =
  name:Yuujinchou.Trie.path option
  -> cbase:check
  -> cfam:check binder
  -> (NbE.Syntax.t -> NbE.Syntax.t -> NbE.Syntax.t)
  -> check

val quantifier : rule
val vir_quantifier : rule
