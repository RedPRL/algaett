open Bwd

module S = Syntax

type env = con Lazy.t bwd (* invariant: lazy values must be effect-less *)
and closure = Clo of {body : S.t; env : env}
and con =
  | Cut of cut
  | Unfold of unfold
  | Pi of con * closure
  | Lambda of closure
  | Sigma of con * closure
  | Pair of con * con
  | Univ of con
  | ULvl of (Mugenjou.Shift.gapped, con) Mugenjou.Syntax.endo
and cut = cut_head * frame bwd
and unfold = unfold_head * frame bwd * con Lazy.t (* invariant: lazy values must be effect-less *)
and cut_head =
  | Lvl of int
  | Axiom of Yuujinchou.Trie.path (* not used for now *)
and unfold_head =
  | Global of Yuujinchou.Trie.path
and frame =
  | App of con
  | Fst
  | Snd

type t = con

let lvl l = Cut (Lvl l, Emp)

module ULvl =
struct
  include Mugenjou.Builder.Endo.Make
      (struct
        module Shift = Mugenjou.Shift.Gapped
        type level = t
        let level l = ULvl l
        let unlevel = function ULvl l -> Some l | _ -> None
      end)

  let rec of_con =
    function
    | Cut (Lvl i, Emp) -> Mugenjou.Syntax.Free.var i
    | ULvl Mugenjou.Syntax.Shifted (l, s) -> Mugenjou.Syntax.Free.shifted (of_con l) s
    | ULvl Mugenjou.Syntax.Top -> Mugenjou.Syntax.Free.top
    | _ -> invalid_arg "of_con"
end
