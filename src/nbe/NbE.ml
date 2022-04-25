module ULvl = ULvl
module Domain = Domain
module Syntax = Syntax

let app = Semantics.app
let fst = Semantics.fst
let snd = Semantics.snd
let inst_clo = Semantics.inst_clo
let inst_clo' = Semantics.inst_clo'
let eval = Semantics.eval

let quote = Quote.con

let force_all = Conversion.force_all
let equate = Conversion.equate
