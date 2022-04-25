type t = Data.syn =
  | Var of int
  | Axiom of Yuujinchou.Trie.path
  | Def of Yuujinchou.Trie.path * Data.value Lazy.t
  | Pi of t * (* binding *) t
  | Lam of (* binding *) t
  | App of t * t
  | Sigma of t * (* binding *) t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of t
  | VirPi of t * (* binding *) t
  | TpULvl
  | ULvl of (Mugenjou.Shift.gapped, t) Mugenjou.Syntax.endo
  | VirUniv

let var v = Var v
let axiom p = Axiom p
let def p v = Def (p, v)
let pi base fam = Pi (base, fam)
let lam b = Lam b
let app t0 t1 = App (t0, t1)
let sigma base fam = Sigma (base, fam)
let pair t0 t1 = Pair (t0, t1)
let fst t = Fst t
let snd t = Snd t
let univ t = Univ t
let vir_pi base fam = VirPi (base, fam)
let tp_ulvl = TpULvl
let ulvl l = ULvl l
let vir_univ = VirUniv

module ULvl =
  Mugenjou.Builder.Endo.Make
    (struct
      module Shift = Mugenjou.Shift.Gapped
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)
