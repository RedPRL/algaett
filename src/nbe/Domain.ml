open Bwd

module S = Syntax

type env = Data.value SyncLazy.t bwd (* invariant: lazy values must be effect-less *)

type closure = Data.closure = Clo of {body : S.t; env : env}

type con = Data.value =
  | Cut of Data.cut
  | Unfold of Data.unfold
  | Pi of con * closure
  | Lam of closure
  | Sigma of con * closure
  | Pair of con * con
  | Univ of con
  | VirPi of con * closure
  | TpULvl
  | ULvl of (Data.ULvlShift.t, con) Mugen.Syntax.endo
  | VirUniv

type cut = Data.cut

type unfold = Data.unfold

type cut_head = Data.cut_head =
  | Lvl of int
  | Axiom of Yuujinchou.Trie.path (* not used for now *)

type unfold_head = Data.unfold_head =
  | Def of Yuujinchou.Trie.path * con SyncLazy.t

type frame = Data.frame =
  | App of con
  | Fst
  | Snd

type t = con
type cell = {tm : t; tp : t}

let lvl l = Cut (Lvl l, Emp)
let def p v = Unfold (Def (p, v), Emp, v)

module ULvl =
  Mugen.Builder.Endo.Make
    (struct
      module Shift = Data.ULvlShift
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)

let univ_top = Univ ULvl.top

let vir_univ = VirUniv
