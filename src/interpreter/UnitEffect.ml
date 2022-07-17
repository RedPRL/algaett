module E = Elaborator
module D = NbE.Domain

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

module Perform : Handler =
struct
  let load p = Effect.perform @@ Load p
  let preload p = Effect.perform @@ Preload p
  let warn_unused i = Effect.perform @@ WarnUnused i
end

open Perform

module P = struct
  type data = Refiner.ResolveData.t
  type tag = Used.id
  type hook = Syntax.empty
  type context = Syntax.empty
end
module S = Yuujinchou.Scope.Make(P)

let include_singleton ?loc (p, data) =
  let id = Used.new_ (Used.Local {value = p; loc}) in
  S.include_singleton (p, (data, id))

let section p =
  S.section p

let get_export () =
  Yuujinchou.Trie.Untagged.untag @@
  S.get_export ()

let import ?loc u m =
  let id = Used.new_ (Imported {value = u; loc}) in
  let u = Yuujinchou.Trie.retag id @@ load u in
  S.import_subtree ~modifier:m ([], u)

module Run (H : Handler) =
struct
  module UsedH : Used.Handler =
  struct
    let warn_unused = warn_unused
  end

  module ElabH : E.Eff.Handler =
  struct
    let counter = ref 0

    let resolve p =
      match S.resolve p with
      | None -> None
      | Some (data, tag) -> Used.use tag; Some data

    let unleash ?loc (name : Syntax.bound_name) data =
      let p =
        match name with
        | Some p -> p
        | None ->
          let i = !counter in
          counter := i + 1;
          ["_"; Int.to_string i]
      in
      include_singleton ?loc (p, data); p
  end

  module UsedR = Used.Run (UsedH)
  module ElabR = E.Eff.Run (ElabH)

  let prerun f =
    UsedR.run @@ fun () ->
    S.run @@ fun () ->
    let ans = ElabR.run f in
    Seq.iter Used.use @@ Yuujinchou.Trie.set_of_tags Used.compare_id @@ S.get_export ();
    ans

  let handler (type a) : a Effect.t -> _ =
    function
    | Load p ->
      Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
      Algaeff.Fun.Deep.finally k @@ fun () -> H.load p
    | Preload p ->
      Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
      Algaeff.Fun.Deep.finally k @@ fun () -> H.preload p
    | WarnUnused i ->
      Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
      Algaeff.Fun.Deep.finally k @@ fun () -> H.warn_unused i
    | _ -> None

  let run f =
    Effect.Deep.try_with prerun f
      {effc = handler}
end
