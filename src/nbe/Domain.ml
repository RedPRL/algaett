open Bwd

module S = Syntax

type env = con bwd
and closure = Clo of {body : S.t; env : env}
and con =
  | Cut of cut
  | Pi of con * closure
  | Lambda of closure
  | Sigma of con * closure
  | Pair of con * con
  | Univ of con
  | ULvl of (Mugenjou.Shift.gapped, con) Mugenjou.Syntax.endo
and cut = head * frame bwd
and head =
  | Lvl of int
  | Global of Yuujinchou.Trie.path
and frame =
  | App of con
  | Fst
  | Snd

type t = con

let lvl l = Cut (Lvl l, Emp)
let global l = Cut (Global l, Emp)

module ULvlBuilder =
  Mugenjou.Builder.Endo.Make
    (struct
      module Shift = Mugenjou.Shift.Gapped
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)
