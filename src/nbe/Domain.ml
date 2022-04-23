open Bwd

module S = Syntax

type env = con bwd
and closure = Clo of {body : S.t; env : env}
and con =
  | Cut of {tp : con; cut : cut}
  | Pi of con * closure
  | Lambda of closure
  | Sigma of con * closure
  | Pair of con * con
  | Univ
and cut = head * frame bwd
and head =
  | Lvl of int
and frame =
  | App of {tp : con; tm : con}
  | Fst
  | Snd

type t = con

let lvl ~tp l = Cut {tp; cut = Lvl l, Emp}
