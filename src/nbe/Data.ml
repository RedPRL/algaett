open Bwd

[@@@warning "-30"]
type syn =
  | Var of int
  | Axiom of Yuujinchou.Trie.path
  | Def of Yuujinchou.Trie.path * value Lazy.t
  | Pi of syn * (* binding *) syn
  | Lam of (* binding *) syn
  | App of syn * syn
  | Sigma of syn * (* binding *) syn
  | Pair of syn * syn
  | Fst of syn
  | Snd of syn
  | Univ of syn
  | VirPi of syn * (* binding *) syn
  | TpULvl
  | ULvl of (Mugen.Shift.linear, syn) Mugen.Syntax.endo
  | VirUniv
and env = value Lazy.t bwd (* invariant: lazy values must be effect-less *)
and closure = Clo of {body : syn; env : env}
and value =
  | Cut of cut
  | Unfold of unfold
  | Pi of value * closure
  | Lam of closure
  | Sigma of value * closure
  | Pair of value * value
  | Univ of value
  | VirPi of value * closure
  | TpULvl
  | ULvl of (Mugen.Shift.linear, value) Mugen.Syntax.endo
  | VirUniv
and cut = cut_head * frame bwd
and unfold = unfold_head * frame bwd * value Lazy.t (* invariant: lazy values must be effect-less *)
and cut_head =
  | Lvl of int
  | Axiom of Yuujinchou.Trie.path
and unfold_head =
  | Def of Yuujinchou.Trie.path * value Lazy.t
and frame =
  | App of value
  | Fst
  | Snd
