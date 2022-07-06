module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl
module LHS = NbE.LHS

module Errors = Errors
module ResolveData = ResolveData
module Eff = Eff
module Tactic = Tactic
module T = Tactic

module ULvl =
struct
  let shifted t i =
    T.Shift.rule @@ fun () ->
    D.ULvl.shifted (T.Shift.run t) (NbE.ULvl.Shift.of_int i)

  let base =
    T.Shift.rule Eff.blessed_ulvl
end

module Structural =
struct
  let local_var (cell : D.cell) =
    T.Infer.rule @@ fun _ ->
    Eff.quote cell.tm, cell.tp

  let global_var path shift =
    T.Infer.rule @@ fun _ ->
    let ulvl = T.Shift.run shift in
    let tm, tp =
      match Eff.resolve path with
      | ResolveData.Axiom {tp} -> S.axiom path, tp
      | ResolveData.Def {tp; tm} -> S.def path tm, tp
    in
    S.app tm (Eff.quote ulvl), NbE.app_ulvl ~tp ~ulvl

  let ann ~ctp ~ctm : T.infer =
    T.Infer.rule @@ fun goal ->
    let tp = Eff.eval @@ T.Check.run {tp = D.univ_top; lhs = LHS.unknown} ctp in
    T.Check.run {tp; lhs = goal.lhs} ctm, tp
end

module Quantifier :
sig
  type rule = name:Yuujinchou.Trie.path option -> cbase:T.check -> cfam:T.check T.binder -> (S.t -> S.t -> S.t) -> T.check

  val quantifier : rule
  val vir_quantifier : rule
end =
struct

  type rule = name:Yuujinchou.Trie.path option -> cbase:T.check -> cfam:T.check T.binder -> (S.t -> S.t -> S.t) -> T.check

  let quantifier ~name ~cbase ~cfam syn : T.check =
    T.Check.rule @@ fun goal ->
    match goal.tp with
    | D.Univ _ ->
      let base = T.Check.run {tp = goal.tp; lhs = LHS.unknown} cbase in
      let vbase = Eff.eval base in
      let fam =
        Eff.bind ~name ~tp:vbase @@ fun x ->
        T.Check.run {tp = goal.tp; lhs = LHS.unknown} @@ cfam x
      in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

  let vir_quantifier ~name ~cbase ~cfam syn : T.check =
    T.Check.rule @@ fun goal ->
    match goal.tp with
    | D.Univ _ ->
      let base = T.Check.run {tp = D.VirUniv; lhs = LHS.unknown} cbase in
      let vbase = Eff.eval base in
      let fam =
        Eff.bind ~name ~tp:vbase @@ fun x ->
        T.Check.run {tp = goal.tp; lhs = LHS.unknown} @@ cfam x
      in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

end

module Sigma =
struct
  let sigma ~name ~cbase ~cfam : T.check =
    Quantifier.quantifier ~name ~cbase ~cfam S.sigma

  let pair ~cfst ~csnd : T.check =
    T.Check.rule @@ fun goal ->
    match goal.tp with
    | D.Sigma (base, fam) ->
      let tm1 = T.Check.run {tp = base; lhs = LHS.fst goal.lhs} cfst in
      let tp2 = NbE.inst_clo fam @@ Eff.lazy_eval tm1 in
      let tm2 = T.Check.run {tp = tp2; lhs = LHS.snd goal.lhs} csnd in
      S.pair tm1 tm2
    | _ ->
      invalid_arg "pair"

  let fst ~itm : T.infer =
    T.Infer.rule @@ fun _ ->
    let tm, tp = T.Infer.run {lhs = LHS.unknown} itm in
    match NbE.force_all tp with
    | D.Sigma (base, _) ->
      S.fst tm, base
    | _ ->
      invalid_arg "fst"

  let snd ~itm : T.infer =
    T.Infer.rule @@ fun _ ->
    let tm, tp = T.Infer.run {lhs = LHS.unknown} itm in
    match NbE.force_all tp with
    | D.Sigma (_, fam) ->
      let tp = NbE.inst_clo fam @@ Eff.lazy_eval @@ S.fst tm in
      S.snd tm, tp
    | _ ->
      invalid_arg "snd"
end

module Pi =
struct
  let pi ~name ~cbase ~cfam : T.check =
    Quantifier.quantifier ~name ~cbase ~cfam S.pi

  let vir_pi ~name ~cbase ~cfam : T.check =
    Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

  let lam ~name ~cbnd : T.check =
    T.Check.rule @@ fun goal ->
    match goal.tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      Eff.bind ~name ~tp:base @@ fun arg ->
      let fib = NbE.inst_clo' fam @@ arg.D.tm in
      S.lam @@ T.Check.run {tp = fib; lhs = LHS.app goal.lhs arg} @@ cbnd arg
    | _ ->
      invalid_arg "lam"

  let app ~itm ~ctm : T.infer =
    T.Infer.rule @@ fun _ ->
    let fn, fn_tp = T.Infer.run {lhs = LHS.unknown} itm in
    match NbE.force_all fn_tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      let arg = T.Check.run {tp = base; lhs = LHS.unknown} ctm in
      let fib = NbE.inst_clo fam @@ Eff.lazy_eval arg in
      S.app fn arg, fib
    | _ ->
      invalid_arg "app"
end

module Univ =
struct
  let univ shift =
    T.Check.rule @@ fun goal ->
    match goal.tp with
    | D.Univ large ->
      let vsmall = T.Shift.run shift in
      if UL.(<) (UL.of_con vsmall) (UL.of_con large)
      then S.univ (Eff.quote vsmall)
      else begin
        let pp_lvl = Mugen.Syntax.Free.dump NbE.ULvl.Shift.dump Format.pp_print_int in
        Format.eprintf "@[<2>Universe@ level@ %a@ is@ not@ smaller@ than@ %a@]@."
          pp_lvl (UL.of_con vsmall)
          pp_lvl (UL.of_con large);
        invalid_arg "univ"
      end
    | _ ->
      invalid_arg "univ"
end
