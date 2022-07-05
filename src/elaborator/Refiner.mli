type infer = unit -> NbE.Syntax.t * NbE.Domain.t
type shift = unit -> NbE.Domain.t
type check = tp:NbE.Domain.t -> NbE.Syntax.t
type 'a binder = NbE.Domain.t -> 'a

module Structural :
sig
  val local_var : RefineEffect.cell -> infer
  val global_var : Yuujinchou.Trie.path -> shift -> infer
  val ann : ctp:check -> ctm:check -> infer
end

module Sigma :
sig
  val sigma : name:Syntax.bound_name -> cbase:check -> cfam:check binder -> check
  val pair : cfst:check -> csnd:check -> check
  val fst : itm:infer -> infer
  val snd : itm:infer -> infer
end

module Pi :
sig
  val pi : name:Syntax.bound_name -> cbase:check -> cfam:check binder -> check
  val vir_pi : name:Syntax.bound_name -> cbase:check -> cfam:check binder -> check
  val lam : name:Syntax.bound_name -> cbnd:check binder -> check
  val app : itm:infer -> ctm:check -> infer
end

module Univ :
sig
  val univ : shift -> check
end