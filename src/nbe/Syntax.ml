type t =
  | Var of int
  | Global of Yuujinchou.Trie.path
  | Pi of t * (* binding *) t
  | Lam of (* binding *) t
  | App of t * t
  | Sigma of t * (* binding *) t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of t
  | TpULvl
  | ULvl of (Mugenjou.Shift.gapped, t) Mugenjou.Syntax.endo

let var v = Var v
let global p = Global p
let pi base fam = Pi (base, fam)
let lam b = Lam b
let app t0 t1 = App (t0, t1)
let sigma base fam = Sigma (base, fam)
let pair t0 t1 = Pair (t0, t1)
let fst t = Fst t
let snd t = Snd t
let univ t = Univ t
let tp_ulvl = TpULvl
let ulvl l = ULvl l
