open Bwd
open BwdNotation

module S = NbE.Syntax
module D = NbE.Domain
module UL = NbE.ULvl

type _ Effect.t += Resolve : Yuujinchou.Trie.path -> ResolveData.t Effect.t

let resolve p = Effect.perform @@ Resolve p

exception Error of Errors.t

let not_convertible u v = raise @@ Error (Conversion (u, v))

let trap f = try Result.ok (f ()) with Error e -> Result.error e

type env = {
  blessed_ulvl : D.t;
  local_names : (D.cell, unit) Yuujinchou.Trie.t;
  locals : D.t Lazy.t bwd;
  size : int;
}

let top_env = {
  blessed_ulvl = D.lvl 0;
  local_names = Yuujinchou.Trie.empty;
  locals = Emp #< (Lazy.from_val @@ D.lvl 0);
  size = 1;
}


module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

let with_top_env f = Eff.run ~env:top_env f

let eval tm = NbE.eval ~env:(Eff.read()).locals tm

let lazy_eval tm =
  let env = (Eff.read()).locals in
  lazy begin NbE.eval ~env tm end

let quote v = NbE.quote ~size:(Eff.read()).size v

let equate v = NbE.equate ~size:(Eff.read()).size v

let resolve_local p = Yuujinchou.Trie.find_singleton p (Eff.read()).local_names

let bind ~name ~tp f =
  let arg = D.lvl (Eff.read()).size in
  let cell = D.{tm = arg; tp} in
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
           (fun _ -> Some (cell, ()))
           env.local_names}
  in
  Eff.scope update @@ fun () -> f cell

let blessed_ulvl () = (Eff.read()).blessed_ulvl

type handler = { resolve : Yuujinchou.Trie.path -> ResolveData.t }

let run f h =
  Effect.Deep.try_with f ()
    { effc =
        fun (type a) (eff : a Effect.t) ->
          match eff with
          | Resolve p ->
            Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
            Algaeff.Fun.Deep.finally k (fun () -> h.resolve p)
          | _ -> None }

let perform : handler = { resolve = resolve }
