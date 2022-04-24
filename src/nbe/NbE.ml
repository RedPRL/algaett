module Domain = Domain
module Syntax = Syntax

let quote_con ~size tm = Quote.con ~size tm
let quote_cut ~size tm = Quote.cut ~size tm
let app = Semantics.app
let fst = Semantics.fst
let snd = Semantics.snd
let inst_clo = Semantics.inst_clo
let eval ~locals ~resolve tm = Semantics.eval ~locals ~resolve tm
