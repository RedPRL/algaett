module ULvl = ULvl
module Domain = Domain
module Syntax = Syntax
module LHS = LHS

let inst_clo = Semantics.inst_clo
let inst_clo' = Semantics.inst_clo'
let eval = Semantics.eval
let eval_top = Semantics.eval_top

let quote = Quote.con

exception Unequal = Conversion.Unequal
let force_all = Conversion.force_all
let equate = Conversion.equate

let app_ulvl ~tp ~ulvl =
  match force_all tp with
  | VirPi (TpULvl, fam) ->
    inst_clo' fam ulvl
  | tp ->
    invalid_arg ("app_ulvl: " ^ Domain.debug_show_head tp)
