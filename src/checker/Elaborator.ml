open Bwd
open BwdNotation

module CS = Syntax
module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type cell = {tm : D.t; tp : D.t}

type env = {
  blessed_ulvl : D.t;
  local_names : (cell, unit) Yuujinchou.Trie.t;
  locals : D.t Lazy.t bwd;
  size : int;
}

let top_env = {
  blessed_ulvl = D.lvl 0;
  local_names = Yuujinchou.Trie.empty;
  locals = Emp #< (Lazy.from_val @@ D.lvl 0);
  size = 1;
}

type resolve_data =
  | Axiom of {tp : NbE.Domain.t}
  | Def of {tm: NbE.Domain.t Lazy.t; tp: NbE.Domain.t}

module Internal =
struct
  type _ Effect.t += Resolve : Yuujinchou.Trie.path -> resolve_data Effect.t

  let resolve p = Effect.perform (Resolve p)

  type error =
    | NotInferable of {tm: Syntax.t}
    | IllTyped of {tm: Syntax.t; tp: D.t}

  exception Error of error

  let not_inferable ~tm = raise (Error (NotInferable {tm}))

  let ill_typed ~tm ~tp = raise (Error (IllTyped {tm; tp}))

  let trap f = try Result.ok (f ()) with Error e -> Result.error e

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)
  (* invariant: the return values must be effect-less *)
  let eval tm = NbE.eval ~env:(Eff.read()).locals tm

  let lazy_eval tm =
    let env = (Eff.read()).locals in
    lazy begin NbE.eval ~env tm end

  let quote v = NbE.quote ~size:(Eff.read()).size v

  let equate v = NbE.equate ~size:(Eff.read()).size v

  let resolve_local p = Yuujinchou.Trie.find_singleton p (Eff.read()).local_names

  let bind ~name ~tp f =
    let arg = D.lvl (Eff.read()).size in
    let update env =
      {blessed_ulvl = env.blessed_ulvl;
       size = env.size + 1;
       locals = env.locals #< (Lazy.from_val arg);
       local_names =
         match name with
         | None -> env.local_names
         | Some name ->
           Yuujinchou.Trie.update_singleton
             name
             (fun _ -> Some ({tm = arg; tp}, ()))
             env.local_names}
    in
    Eff.scope update @@ fun () -> f arg

  let blessed_ulvl () = (Eff.read()).blessed_ulvl
end

let elab_shift : Syntax.shift -> UL.Shift.t =
  function
  | CS.Translate i -> NbE.ULvl.Shift.of_int i

let shifted_blessed_ulvl : Syntax.shift list option -> D.t =
  function
  | None -> Internal.blessed_ulvl ()
  | Some ss ->
    List.fold_right (fun s l -> D.ULvl.shifted l (elab_shift s)) ss (Internal.blessed_ulvl ())

type infer = unit -> S.t * D.t
type check = tp:D.t -> S.t
type 'a binder = D.t -> 'a

module Structural : sig
  val var : Syntax.name -> Syntax.shift list option -> infer
  val ann : ctp:check -> ctm:check -> infer
end =
struct

  let var p s : infer = fun () ->
    match Internal.resolve_local p, s with
    | Some ({tm; tp}, ()), None -> Internal.quote tm, tp
    | Some _, Some _ ->
      Format.eprintf "@[<2>Local@ variable@ %a@ could@ not@ have@ level@ shifting@]@." Syntax.dump_name p;
      Internal.not_inferable ~tm:{node = CS.Var (p, s); loc = None}
    | None, _ ->
      let ulvl = shifted_blessed_ulvl s in
      let tm, tp =
        match Internal.resolve p with
        | Axiom {tp} -> S.axiom p, tp
        | Def {tp; tm} -> S.def p tm, tp
      in
      S.app tm (Internal.quote ulvl), NbE.app_ulvl ~tp ~ulvl

  let ann ~ctp ~ctm : infer = fun () ->
    let tp = Internal.eval @@ ctp ~tp:D.univ_top in
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
      let fam = Internal.bind ~name ~tp:(Internal.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

  let vir_quantifier ~name ~cbase ~cfam syn : check = fun ~tp ->
    match tp with
    | D.Univ _ ->
      let base = cbase ~tp:D.VirUniv in
      let fam = Internal.bind ~name ~tp:(Internal.eval base) @@ fun x -> cfam x ~tp in
      syn base fam
    | _ ->
      invalid_arg "quantifier"

end

module Sigma : sig
  val sigma : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val pair : cfst:check -> csnd:check -> check
  val fst : itm:infer -> infer
  val snd : itm:infer -> infer
end =
struct

  let sigma ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.sigma

  let pair ~cfst ~csnd : check = fun ~tp ->
    match tp with
    | D.Sigma (base, fam) ->
      let tm1 = cfst ~tp:base in
      let tp2 = NbE.inst_clo fam @@ Internal.lazy_eval tm1 in
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
      let tp = NbE.inst_clo fam @@ Internal.lazy_eval @@ S.fst tm in
      S.snd tm, tp
    | _ ->
      invalid_arg "snd"

end

module Pi : sig
  val pi : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val vir_pi : name:CS.bound_name -> cbase:check -> cfam:check binder -> check
  val lam : name:CS.bound_name -> cbnd:check binder -> check
  val app : itm:infer -> ctm:check -> infer
end =
struct

  let pi ~name ~cbase ~cfam : check =
    Quantifier.quantifier ~name ~cbase ~cfam S.pi

  let vir_pi ~name ~cbase ~cfam : check =
    Quantifier.vir_quantifier ~name ~cbase ~cfam S.pi

  let lam ~name ~cbnd : check = fun ~tp ->
    match tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      Internal.bind ~name ~tp:base @@ fun arg ->
      S.lam @@ cbnd arg ~tp:(NbE.inst_clo' fam arg)
    | _ ->
      failwith ""

  let app ~itm ~ctm : infer = fun () ->
    let fn, fn_tp = itm () in
    match NbE.force_all fn_tp with
    | D.Pi (base, fam) | D.VirPi (base, fam) ->
      let arg = ctm ~tp:base in
      let fib = NbE.inst_clo fam @@ Internal.lazy_eval arg in
      S.app fn arg, fib
    | _ ->
      invalid_arg "app"

end

module Univ : sig
  val univ : Syntax.shift list option -> check
end =
struct
  let univ s ~tp =
    match tp with
    | D.Univ large ->
      let vsmall = shifted_blessed_ulvl s in
      if UL.(<) (UL.of_con vsmall) (UL.of_con large)
      then S.univ (Internal.quote vsmall)
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

let rec infer tm : infer = fun () ->
  match tm.CS.node with
  | CS.Var (p, s) ->
    Structural.var p s ()
  | CS.Ann {tm; tp} ->
    Structural.ann ~ctp:(check tp) ~ctm:(check tm) ()
  | CS.App (tm1, tm2) ->
    Pi.app ~itm:(infer tm1) ~ctm:(check tm2) ()
  | CS.Fst tm ->
    Sigma.fst ~itm:(infer tm) ()
  | CS.Snd tm ->
    Sigma.snd ~itm:(infer tm) ()
  | _ ->
    (* Format.eprintf "@[<2>Could@ not@ infer@ the@ type@ of@ %a@]@." Syntax.dump tm; *)
    Internal.not_inferable ~tm

(* The [fallback_infer] parameter is for the two-stage type checking: first round,
   we try to check things without unfolding the type, and then we unfold the type
   if type inference also fails. During the second round, we do not want to try
   the type inference again becouse it will have already failed once. *)
and check ?(fallback_infer=true) tm : check = fun ~tp ->
  match tm.CS.node with
  | CS.Pi (base, name, fam) ->
    Pi.pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.VirPi (base, name, fam) ->
    Pi.vir_pi ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Sigma (base, name, fam) ->
    Sigma.sigma ~name ~cbase:(check base) ~cfam:(fun _ -> check fam) ~tp
  | CS.Lam (name, body) ->
    Pi.lam ~name ~cbnd:(fun _ -> check body) ~tp
  | CS.Pair (tm1, tm2) ->
    Sigma.pair ~cfst:(check tm1) ~csnd:(check tm2) ~tp
  | CS.Univ s ->
    Univ.univ s ~tp
  | _ when fallback_infer ->
    begin
      try
        match infer tm () with
        | tm', tp' ->
          begin
            try Internal.equate tp' `LE tp; tm' with
            | NbE.Unequal -> Internal.ill_typed ~tm ~tp
          end
      with Internal.Error (NotInferable _) ->
      match tp with
      | D.Unfold _ ->
        check ~fallback_infer:false tm ~tp:(NbE.force_all tp)
      | _ ->
        Internal.ill_typed ~tm ~tp
    end
  | _ ->
    Internal.ill_typed ~tm ~tp

(* the public interface *)

type error = Internal.error =
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: D.t}

let infer_top tm =
  Internal.trap @@ fun () ->
  let tm, tp =
    Internal.Eff.run ~env:top_env @@ fun () ->
    let tm, tp = infer tm () in tm, Internal.quote tp
  in
  S.lam tm, NbE.eval_top (S.vir_pi S.tp_ulvl tp)

let check_tp_top tp =
  Internal.trap @@ fun () ->
  let tp = Internal.Eff.run ~env:top_env @@ fun () -> check tp ~tp:D.univ_top in
  S.vir_pi S.tp_ulvl tp

let check_top tm ~tp =
  Internal.trap @@ fun () ->
  S.lam @@
  Internal.Eff.run ~env:top_env @@ fun () ->
  check tm ~tp:(NbE.app_ulvl ~tp ~ulvl:(Internal.blessed_ulvl ()))

type handler = { resolve : Yuujinchou.Trie.path -> resolve_data }

let run f h =
  Effect.Deep.try_with f ()
    { effc =
        fun (type a) (eff : a Effect.t) ->
          match eff with
          | Internal.Resolve p ->
            Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
            Algaeff.Fun.Deep.finally k (fun () -> h.resolve p)
          | _ -> None }

let perform : handler = { resolve = Internal.resolve }
