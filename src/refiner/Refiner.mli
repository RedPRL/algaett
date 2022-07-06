module Errors : module type of Errors
module Eff : module type of Eff
module ResolveData : module type of ResolveData

module Rule : sig
  type infer
  type shift
  type check
  type hyp = NbE.Domain.cell

  type 'a binder = hyp -> 'a

  module Infer : Sigs.InferPublic with type t = infer
  module Check : Sigs.CheckPublic with type t = check and type infer := Infer.t
  module Shift : Sigs.ShiftPublic with type t = shift
end

open Rule

module ULvl :
sig
  val base : shift
  val shifted : shift -> int -> shift
end

module Structural :
sig
  val local_var : hyp -> infer
  val global_var : Yuujinchou.Trie.path -> shift -> infer
  val ann : ctp:check -> ctm:check -> infer
end

module Sigma :
sig
  val sigma : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
  val pair : cfst:check -> csnd:check -> check
  val fst : itm:infer -> infer
  val snd : itm:infer -> infer
end

module Pi :
sig
  val pi : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
  val vir_pi : name:Yuujinchou.Trie.path option -> cbase:check -> cfam:check binder -> check
  val lam : name:Yuujinchou.Trie.path option -> cbnd:check binder -> check
  val app : itm:infer -> ctm:check -> infer
end

module Univ :
sig
  val univ : shift -> check
end
