module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

module Errors = Errors
module ResolveData = ResolveData
module Hyp = RefineEffect.Cell

module R = Rule

module Shift =
struct

  let shifted t i =
    R.Shift.rule @@ fun () ->
    D.ULvl.shifted (R.Shift.run t) (NbE.ULvl.Shift.of_int i)

  let base = R.Shift.rule RefineEffect.blessed_ulvl
end

module Structural =
struct
  let local_var cell =
    R.Infer.rule @@ fun () ->
    RefineEffect.quote cell.RefineEffect.tm, cell.RefineEffect.tp

  let global_var path shift =
    R.Infer.rule @@ fun () ->
    let ulvl = R.Shift.run shift in
    let tm, tp =
      match RefineEffect.resolve path with
      | ResolveData.Axiom {tp} -> S.axiom path, tp
      | ResolveData.Def {tp; tm} -> S.def path tm, tp
    in
    S.app tm (RefineEffect.quote ulvl), NbE.app_ulvl ~tp ~ulvl

  let ann ~ctp ~ctm : R.infer =
    R.Infer.rule @@ fun () ->
    let tp = RefineEffect.eval @@ R.Check.run ~tp:D.univ_top ctp in
    R.Check.run ~tp ctm, tp
end

module Quantifier :
sig
  type rule = name:Yuujinchou.Trie.path option -> cbase:R.check -> cfam:R.check R.binder -> (S.t -> S.t -> S.t) -> R.check

  val quantifier : rule
  val vir_quantifier : rule
end =
struct

  type rule = name:Yuujinchou.Trie.path option -> cbase:R.check -> cfam:R.check R.binder -> (S.t -> S.t -> S.t) -> R.check

  let quantifier ~name ~cbase ~cfam syn : R.check =
    R.Check.rule @@ fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = R.Check.run ~tp cbase in
      let vbase = RefineEffect.eval base in
      let fam = RefineEffect.bind ~name ~tp:vbase @@ fun x -> R.Check.run ~tp @@ cfam x in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

  let vir_quantifier ~name ~cbase ~cfam syn : R.check =
    R.Check.rule @@ fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = R.Check.run ~tp:D.VirUniv cbase in
      let vbase = RefineEffect.eval base in
      let fam = RefineEffect.bind ~name ~tp:vbase @@ fun x -> R.Check.run ~tp @@ cfam x in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

end

module Sigma =
struct
  let sigma ~name ~cbase ~cfam : R.check =
    Quantifier.quantifier ~name ~cbase ~cfam S.sigma

  let pair ~cfst ~csnd : R.check =
    R.Check.rule @@ fun ~tp ->
    match tp with
    | D.Sigma (base, fam) ->
      let tm1 = R.Check.run ~tp:base cfst in
      let tp2 = NbE.inst_clo fam @@ RefineEffect.lazy_eval tm1 in
      let tm2 = R.Check.run ~tp:tp2 csnd in
      S.pair tm1 tm2
    | _ ->
      invalid_arg "pair"

  let fst ~itm : R.infer =
    R.Infer.rule @@ fun () ->
    let tm, tp = R.Infer.run itm in
    match NbE.force_all tp with
    | D.Sigma (base, _) ->
      S.fst tm, base
    | _ ->
      invalid_arg "fst"

  let snd ~itm : R.infer =
    R.Infer.rule @@ fun () ->
    let tm, tp = R.Infer.run itm in
    match NbE.force_all tp with
    | D.Sigma (_, fam) ->
      let tp = NbE.inst_clo fam @@ RefineEffect.lazy_eval @@ S.fst tm in
      S.snd tm, tp
    | _ ->
      invalid_arg "snd"
end

module Pi =
struct
  let pi ~name ~cbase ~cfam : R.check =
    Quantifier.quantifier ~name ~cbase ~cfam S.pi

  let vir_pi ~name ~cbase ~cfam : R.check =
    Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

  let lam ~name ~cbnd : R.check =
    R.Check.rule @@ fun ~tp ->
    match tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      RefineEffect.bind ~name ~tp:base @@ fun arg ->
      let fib = NbE.inst_clo' fam @@ Hyp.tm arg in
      S.lam @@ R.Check.run ~tp:fib @@ cbnd arg
    | _ ->
    invalid_arg "lam"

  let app ~itm ~ctm : R.infer =
    R.Infer.rule @@ fun () ->
    let fn, fn_tp = R.Infer.run itm in
    match NbE.force_all fn_tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      let arg = R.Check.run ~tp:base ctm in
      let fib = NbE.inst_clo fam @@ RefineEffect.lazy_eval arg in
      S.app fn arg, fib
    | _ ->
      invalid_arg "app"
end

module Univ =
struct
  let univ shift =
    R.Check.rule @@ fun ~tp ->
    match tp with
    | D.Univ large ->
      let vsmall = R.Shift.run shift in
      if UL.(<) (UL.of_con vsmall) (UL.of_con large)
      then S.univ (RefineEffect.quote vsmall)
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
