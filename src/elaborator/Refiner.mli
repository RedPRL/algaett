type infer
type shift
type check
type hyp = RefineEffect.cell

type 'a binder = hyp -> 'a

module Infer :
sig
  type t = infer
  val run : t -> NbE.Syntax.t * NbE.Domain.t
end

module Check :
sig
  type t = check
  val run : tp:NbE.Domain.t -> t -> NbE.Syntax.t
  val peek : (tp:NbE.Domain.t -> t) -> t
  val orelse : t -> (Errors.t -> t) -> t
  val infer : infer -> t
  val forcing : t -> t
end

module Shift :
sig
  type t = shift
  val blessed : t
  val shifted : t -> int -> t
  val run : t -> NbE.Domain.t
end

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