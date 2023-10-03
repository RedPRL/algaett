[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Locate
open Token

module E = Earley_core.Earley
module S = Elaborator.Syntax

let virtual_tp = located term_vir_tp

let parser bound_name =
  | underscore -> None
  | x:name -> Some x

let parser atomic_term_ =
  | "{" term_ "}"
  | x:name -> S.Var (x, None)
  | x:name up s:shift -> S.Var (x, Some s)
  | c:term_constant -> c ~shift:None
  | c:term_constant up s:shift -> c ~shift:(Some s)
  | question -> S.Hole
and atomic_term = located atomic_term_
and parser atomic_shift =
  | plus i:pos_num -> S.Translate i
  | ps:(E.greedy @@ E.list1 plus (E.empty ())) -> S.Translate (List.length ps)
and parser shift =
  | i:pos_num -> [S.Translate i]
  | "{" (E.list0 atomic_shift (E.empty ())) "}"
  | "{" d:pos_num ss:(E.list0 atomic_shift (E.empty ())) "}" -> (S.Translate d)::ss
and parser app_term_ =
  | atomic_term_
  | f:app_term arg:atomic_term ->
      S.App (f, arg)
  | tm:atomic_term at proj:term_field ->
      proj tm
and app_term = located app_term_
and parser term_ =
  | app_term_
  | tm:app_term colon tp:term ->
      S.Ann {tm; tp}
  | "(" x:bound_name colon base:term ")" right_arrow fam:term ->
      S.Pi (base, x, fam)
  | base:app_term right_arrow fam:term ->
      S.Pi (base, None, fam)
  | "(" x:bound_name colon base:virtual_tp ")" right_arrow fam:term ->
      S.VirPi (base, x, fam)
  | base:virtual_tp right_arrow fam:term ->
      S.VirPi (base, None, fam)
  | x:bound_name maps_to tm:term ->
      S.Lam (x, tm)
  | "(" x:bound_name colon base:term ")" times fam:term ->
      S.Sigma (base, x, fam)
  | base:app_term times fam:term ->
      S.Sigma (base, None, fam)
  | "[" t1:term comma t2:term "]" ->
      S.Pair (t1, t2)
and term = located term_
