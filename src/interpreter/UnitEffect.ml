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

module S =
  Yuujinchou.Scope.Make
    (struct
      type data = Refiner.ResolveData.t
      type tag = Used.id
      type hook = Syntax.empty
      type context = Syntax.empty
    end)


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
  let u = S.modify m @@ Yuujinchou.Trie.retag id @@ load u in
  S.import_subtree ([], u)

module Run (H : Handler) =
struct
  module UsedH : Used.Handler =
  struct
    let warn_unused = warn_unused
  end

  module ElabH : E.Eff.Handler =
  struct
    let counter = ref 0

    let resolve Asai.Span.{value = p ; loc} =
      match S.resolve p with
      | None -> 
        Error.Logger.fatalf ?loc ~code:NotInScope "The variable '%a' is not in scope" Elaborator.Syntax.dump_name p
      | Some (data, tag) -> Used.use tag; data

    let unleash (name : Syntax.bound_name) data =
      let p =
        match name with
        | Some p -> p
        | None ->
          let i = !counter in
          counter := i + 1;
          ["_"; Int.to_string i]
      in
      include_singleton (p, data); p
  end

  module ScopeH : S.Handler =
  struct
    let not_found _ _ = ()
    let shadow _ _ _ y = y
    let hook _ _ : Syntax.empty -> _ =
      function _ -> .
  end

  module UsedR = Used.Run (UsedH)
  module ElabR = E.Eff.Run (ElabH)
  module ScopeR = S.Run (ScopeH)

  let prerun f =
    UsedR.run @@ fun () ->
    ScopeR.run @@ fun () ->
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
