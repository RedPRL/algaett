module Domain = Domain
module Syntax = Syntax

let app = Semantics.app
let fst = Semantics.fst
let snd = Semantics.snd
let inst_clo = Semantics.inst_clo
let inst_clo' = Semantics.inst_clo'
let eval = Semantics.eval

let quote_con = Quote.con
let quote_cut = Quote.cut

let equate = Conversion.equate
