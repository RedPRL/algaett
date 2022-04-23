open Bwd

module Domain = Domain
module Syntax = Syntax

module Internal =
struct
  type cell = {tp : Domain.t; tm : Domain.t}
  type env = cell bwd
  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let size () = BwdLabels.length (Eff.read ())
  let eval_env () = BwdLabels.map ~f:(fun x -> x.tm) (Eff.read ())
end

type cell = Internal.cell = {tp : Domain.t; tm : Domain.t}
type env = Internal.env

let quote_con ~tp tm = Quote.con ~size:(Internal.size()) ~tp tm
let quote_cut tm = Quote.cut ~size:(Internal.size()) tm
let app = Semantics.app
let fst = Semantics.fst
let snd = Semantics.snd
let inst_clo = Semantics.inst_clo
let eval tm = Semantics.eval ~env:(Internal.eval_env()) tm
let run = Internal.Eff.run
