open Bwd

module S = Syntax

type env = Data.value Lazy.t bwd (* invariant: lazy values must be effect-less *)

type closure = Data.closure = Clo of {body : S.t; env : env}
type con = Data.value =
  | Cut of Data.cut
  | Unfold of Data.unfold
  | Pi of con * closure
  | Lam of closure
  | Sigma of con * closure
  | Pair of con * con
  | Univ of con
  | TpULvl
  | ULvl of (Mugenjou.Shift.gapped, con) Mugenjou.Syntax.endo
type cut = Data.cut
type unfold = Data.unfold
type cut_head = Data.cut_head =
  | Lvl of int
  | Axiom of Yuujinchou.Trie.path (* not used for now *)
type unfold_head = Data.unfold_head =
  | Def of Yuujinchou.Trie.path * con
type frame = Data.frame =
  | App of con
  | Fst
  | Snd

type t = con

let lvl l = Cut (Lvl l, Emp)
let def p v = Unfold (Def (p, v), Emp, Lazy.from_val v)

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
    | Unfold (_, _, v) -> of_con (Lazy.force v)
    | _ -> invalid_arg "of_con"
end
