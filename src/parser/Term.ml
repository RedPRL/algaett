[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Locate
open Token

module S = Checker.Syntax

let virtual_tp = located term_vir_tp

let parser bound_name =
  | underscore -> None
  | x:name -> Some x

let parser atomic_term_ =
  | "{" term_ "}"
  | x:name -> S.Var x
and atomic_term = located atomic_term_
and parser shift =
  | s:num -> [s]
  | "{" ss:(Earley_core.Earley.list0 num comma) "}" -> ss
and parser app_term_ =
  | atomic_term_
  | f:term_fun1 arg:atomic_term ->
      f arg
  | f:app_term arg:atomic_term ->
      S.App (f, arg)
  | tm:atomic_term at proj:term_field ->
      proj tm
  | tm:app_term up s:shift ->
      S.ULvlShifted (tm, s)
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
