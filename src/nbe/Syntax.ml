type t =
  | Var of int
  | Pi of t * (* binding *) t
  | Lambda of (* binding *) t
  | App of t * t
  | Sigma of t * (* binding *) t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ

let var v = Var v
let pi base fam = Pi (base, fam)
let lam b = Lambda b
let app t0 t1 = App (t0, t1)
let sigma base fam = Sigma (base, fam)
let pair t0 t1 = Pair (t0, t1)
let fst t = Fst t
let snd t = Snd t
