open Bwd
open Bwd.Infix

module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type _ Effect.t += Resolve : Yuujinchou.Trie.path -> ResolveData.t option Effect.t

module type Handler =
sig
  val resolve : Yuujinchou.Trie.path -> ResolveData.t option
end

module Run (H : Handler) =
struct
  let run f =
    Effect.Deep.try_with f ()
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Resolve p ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k (fun () -> H.resolve p)
            | _ -> None }
end

module Perform =
struct
  let resolve p = Effect.perform @@ Resolve p
end

include Perform

type env = {
  blessed_ulvl : D.t;
  local_names : D.cell Yuujinchou.Trie.Untagged.t;
  locals : D.cell SyncLazy.t bwd;
  size : int;
}

let top_env = {
  blessed_ulvl = D.lvl 0;
  local_names = Yuujinchou.Trie.empty;
  locals = Emp <: SyncLazy.from_val @@ D.{tm = D.lvl 0; tp = D.TpULvl};
  size = 1;
}


module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

let with_top_env f = Eff.run ~env:top_env f

let nbe_env () =
  (Eff.read()).locals |> Bwd.map @@ SyncLazy.map @@ fun cell -> cell.D.tm

let eval tm =
  let env = nbe_env () in
  NbE.eval ~env tm

let lazy_eval tm =
  let env = nbe_env () in
  SyncLazy.from_lazy @@ lazy begin NbE.eval ~env tm end

let quote v =
  NbE.quote ~size:(Eff.read()).size v

let equate v =
  NbE.equate ~size:(Eff.read()).size v

let resolve_local p =
  Yuujinchou.Trie.Untagged.find_singleton p (Eff.read()).local_names

let resolve_level lvl =
  let env = (Eff.read()).locals in
  Option.map SyncLazy.force @@ Bwd.nth_opt env lvl

let bind ~name ~tp f =
  let arg = D.lvl (Eff.read()).size in
  let cell = D.{tm = arg; tp} in
  let update env =
    {blessed_ulvl = env.blessed_ulvl;
     size = env.size + 1;
     locals = env.locals <: SyncLazy.from_val cell;
     local_names =
       match name with
       | None -> env.local_names
       | Some name ->
         Yuujinchou.Trie.update_singleton
           name
           (fun _ -> Some (cell, ()))
           env.local_names}
  in
  Eff.scope update @@ fun () -> f cell

let blessed_ulvl () =
  (Eff.read()).blessed_ulvl

module Generalize =
struct
  type bnd = VirType of S.t | Type of S.t

  let fold_right_ctx ~nil:(nil : unit -> 'a) ~cons:(cons : arg:S.t -> bnd:bnd -> 'a -> 'a) =
    let rec loop =
      function
      | [] -> nil ()
      | lcell :: env ->
        let cell = SyncLazy.force lcell in
        let arg = quote cell.D.tm in
        let bnd =
          let tp = quote cell.D.tp in
          match NbE.force_all cell.D.tp with
          | D.TpULvl -> VirType tp
          | _ -> Type tp
        in
        bind ~name:None ~tp:cell.D.tp @@ fun cell ->
        cons ~arg ~bnd @@ loop env
    in
    (* Get the local environment, but chop off the blessed universe level. *)
    let env = List.tl @@ Bwd.to_list @@ (Eff.read()).locals in
    with_top_env @@ fun () ->
    loop env

  let quote_ctx () : bnd list =
    let nil () = [] in
    let cons ~arg ~bnd bnds = bnd :: bnds in
    fold_right_ctx ~nil ~cons
end
