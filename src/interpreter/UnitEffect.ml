type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm : Syntax.t}
  | IllTyped of {tm : Syntax.t; tp : NbE.Domain.t}
  | Conversion of NbE.Domain.t * NbE.Domain.t

exception Error of error

let trap f = try Result.ok (f ()) with Error e -> Result.error e

let reraise_elaborator =
  function
  | Ok v -> v
  | Error (Elaborator.Errors.NotInferable {tm}) -> raise (Error (NotInferable {tm}))
  | Error (Elaborator.Errors.IllTyped {tm; tp}) -> raise (Error (IllTyped {tm; tp}))
  | Error (Elaborator.Errors.Conversion (u, v)) -> raise (Error (Conversion (u, v)))


let not_in_scope n = raise (Error (NotInScope n))

type _ Effect.t +=
  | Load : Bantorra.Manager.path -> Refiner.ResolveData.t Yuujinchou.Trie.Untagged.t Effect.t
  | Preload : Bantorra.Manager.path -> unit Effect.t
  | WarnUnused : Used.info -> unit Effect.t

module type Handler =
sig
  val load : Bantorra.Manager.path -> Refiner.ResolveData.t Yuujinchou.Trie.Untagged.t
  val preload : Bantorra.Manager.path -> unit
  val warn_unused : Used.info -> unit
end

let load p =
  Effect.perform (Load p)

let preload p =
  Effect.perform (Preload p)

let warn_unused i =
  Effect.perform (WarnUnused i)

module Perform : Handler =
struct
  let load = load
  let preload = preload
  let warn_unused = warn_unused
end

module S =
  Yuujinchou.Scope.Make
    (struct
      type data = Refiner.ResolveData.t
      type tag = Used.id
      type hook = Syntax.empty
      type context = Syntax.empty
    end)

let include_singleton ?loc (p, data) =
  let id = Used.new_ (Used.Local {node = p; loc}) in
  S.include_singleton (p, (data, id))

let section p = S.section p

let get_export () = Yuujinchou.Trie.Untagged.untag @@ S.get_export ()

let import ?loc u m =
  let id = Used.new_ (Imported {node = u; loc}) in
  let u = S.modify m @@ Yuujinchou.Trie.retag id @@ load u in
  S.import_subtree ([], u)

module UsedHandler =
struct
  let warn_unused = warn_unused
end

let run_used f =
  let module R = Used.Run (UsedHandler) in
  R.run f

let run_scope f =
  S.run
    (fun () ->
       let ans = f () in
       Seq.iter Used.use @@ Yuujinchou.Trie.set_of_tags Used.compare_id @@ S.get_export ();
       ans)
    { not_found = (fun _ _ -> ());
      shadow = (fun _ _ _ y -> y);
      hook = (fun _ _ -> function _ -> .) }

module ElaboratorHandler =
struct
  let resolve p =
    match S.resolve p with
    | None -> not_in_scope p
    | Some (data, tag) -> Used.use tag; data
end

let run_checker f =
  let module R = Elaborator.Run (ElaboratorHandler) in
  R.run f

module Run (H : Handler) =
struct
  let run f =
    Effect.Deep.try_with
      run_used (fun () -> run_scope @@ fun () -> run_checker f)
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Load p ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> H.load p
            | Preload p ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> H.preload p
            | WarnUnused i ->
              Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> H.warn_unused i
            | _ -> None }
end
