open Bwd

module Domain = Domain
module Syntax = Syntax

module Internal =
struct
  type env =
    { eval : Semantics.env
    ; size : int
    }
  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let make_env ~locals ~resolve =
    { eval = {locals; resolve}
    ; size = BwdLabels.length locals
    }
  let size () = (Eff.read()).size
  let eval_env () = (Eff.read()).eval
end

let quote_con tm = Quote.con ~size:(Internal.size()) tm
let quote_cut tm = Quote.cut ~size:(Internal.size()) tm
let app = Semantics.app
let fst = Semantics.fst
let snd = Semantics.snd
let inst_clo = Semantics.inst_clo
let eval tm = Semantics.eval ~env:(Internal.eval_env()) tm
let run ~locals ~resolve = Internal.Eff.run ~env:(Internal.make_env ~locals ~resolve)
