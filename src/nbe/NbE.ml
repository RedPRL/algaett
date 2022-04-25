module ULvl = ULvl
module Domain = Domain
module Syntax = Syntax

let inst_clo = Semantics.inst_clo
let inst_clo' = Semantics.inst_clo'
let eval = Semantics.eval
let eval_top = Semantics.eval_top

let quote = Quote.con

let force_all = Conversion.force_all
let equate = Conversion.equate
