open Bwd

module ULvlShift = Mugen.Shift.Int

type global = Bantorra.Manager.path * int

[@@@warning "-30"]
type syn =
  | Var of int
  | Axiom of Yuujinchou.Trie.path
  | Def of Yuujinchou.Trie.path * value SyncLazy.t
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
  | ULvl of (ULvlShift.t, syn) Mugen.Syntax.endo
  | VirUniv

and env = value SyncLazy.t bwd (* invariant: lazy values must be effect-less *)

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
  | ULvl of (ULvlShift.t, value) Mugen.Syntax.endo
  | VirUniv

and cut = cut_head * frame bwd

and unfold = unfold_head * frame bwd * value SyncLazy.t (* invariant: lazy values must be effect-less *)

and cut_head =
  | Lvl of int
  | Axiom of Yuujinchou.Trie.path

and unfold_head =
  | Def of Yuujinchou.Trie.path * value SyncLazy.t

and frame =
  | App of value
  | Fst
  | Snd
