module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

module Syntax = CS
module Errors = Errors
module ResolveData = ResolveData

type infer = unit -> S.t * D.t
type shift = unit -> D.t
type check = tp:D.t -> S.t
type 'a binder = D.t -> 'a

module Structural = struct
  let local_var cell = fun () ->
    RefineEffect.quote cell.RefineEffect.tm, cell.RefineEffect.tp

  let global_var path shift = fun () ->
    let ulvl = shift () in
    let tm, tp =
      match RefineEffect.resolve path with
      | ResolveData.Axiom {tp} -> S.axiom path, tp
      | ResolveData.Def {tp; tm} -> S.def path tm, tp
    in
    S.app tm (RefineEffect.quote ulvl), NbE.app_ulvl ~tp ~ulvl

  let ann ~ctp ~ctm : infer = fun () ->
    let tp = RefineEffect.eval @@ ctp ~tp:D.univ_top in
    ctm ~tp, tp
end

module Quantifier : sig
  type rule = name:CS.bound_name -> cbase:check -> cfam:check binder -> (S.t -> S.t -> S.t) -> check

  val quantifier : rule
  val vir_quantifier : rule
end =
struct

  type rule = name:CS.bound_name -> cbase:check -> cfam:check binder -> (S.t -> S.t -> S.t) -> check

  let quantifier ~name ~cbase ~cfam syn : check = fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = cbase ~tp in
      let fam = RefineEffect.bind ~name ~tp:(RefineEffect.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

  let vir_quantifier ~name ~cbase ~cfam syn : check = fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = cbase ~tp:D.VirUniv in
      let fam = RefineEffect.bind ~name ~tp:(RefineEffect.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

end

module Sigma =
struct
  let sigma ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.sigma

  let pair ~cfst ~csnd : check = fun ~tp ->
    match tp with
    | D.Sigma (base, fam) ->
      let tm1 = cfst ~tp:base in
      let tp2 = NbE.inst_clo fam @@ RefineEffect.lazy_eval tm1 in
      let tm2 = csnd ~tp:tp2 in
      S.pair tm1 tm2
    | _ ->
      invalid_arg "pair"

  let fst ~itm : infer = fun () ->
    let tm, tp = itm () in
    match NbE.force_all tp with
    | D.Sigma (base, _) ->
      S.fst tm, base
    | _ ->
      invalid_arg "fst"

  let snd ~itm : infer = fun () ->
    let tm, tp = itm () in
    match NbE.force_all tp with
    | D.Sigma (_, fam) ->
      let tp = NbE.inst_clo fam @@ RefineEffect.lazy_eval @@ S.fst tm in
      S.snd tm, tp
    | _ ->
      invalid_arg "snd"
end

module Pi =
struct
  let pi ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.pi

  let vir_pi ~name ~cbase ~cfam : check =
    Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

  let lam ~name ~cbnd : check = fun ~tp ->
    match tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      RefineEffect.bind ~name ~tp:base @@ fun arg ->
      S.lam @@ cbnd arg ~tp:(NbE.inst_clo' fam arg)
    | _ ->
      failwith ""

  let app ~itm ~ctm : infer = fun () ->
    let fn, fn_tp = itm () in
    match NbE.force_all fn_tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      let arg = ctm ~tp:base in
      let fib = NbE.inst_clo fam @@ RefineEffect.lazy_eval arg in
      S.app fn arg, fib
    | _ ->
      invalid_arg "app"
end

module Univ =
struct
  let univ shift ~tp =
    match tp with
    | D.Univ large ->
      let vsmall = shift () in
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