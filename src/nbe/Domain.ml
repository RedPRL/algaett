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
  | Univ
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
