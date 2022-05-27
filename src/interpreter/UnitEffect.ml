type error =
  | NotInScope of Yuujinchou.Trie.path
  | NotInferable of {tm: Syntax.t}
  | IllTyped of {tm: Syntax.t; tp: NbE.Domain.t}
exception Error of error
let trap f = try Result.ok (f ()) with Error e -> Result.error e
let reraise_checker =
  function
  | Ok v -> v
  | Error (Checker.NotInferable {tm}) -> raise (Error (NotInferable {tm}))
  | Error (Checker.IllTyped {tm; tp}) -> raise (Error (IllTyped {tm; tp}))
let not_in_scope n = raise (Error (NotInScope n))

exception%effect Load : Bantorra.Manager.path -> Checker.resolve_data Yuujinchou.Trie.Untagged.t
exception%effect Preload : Bantorra.Manager.path -> unit
exception%effect WarnUnused : Used.info -> unit
type handler =
  { load : Bantorra.Manager.path -> Checker.resolve_data Yuujinchou.Trie.Untagged.t;
    preload : Bantorra.Manager.path -> unit;
    warn_unused : Used.info -> unit }
let load p = Effect.perform (Load p)
let preload p = Effect.perform (Preload p)
let warn_unused i = Effect.perform (WarnUnused i)
let perform : handler = { load; preload; warn_unused }

module S = Yuujinchou.Scope.Make
    (struct
      type data = Checker.resolve_data
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

let run_used f = Used.run f { warn_unused }

let run_scope f =
  S.run
    (fun () ->
       let ans = f () in
       Seq.iter Used.use @@ Yuujinchou.Trie.set_of_tags Used.compare_id @@ S.get_export ();
       ans)
    { not_found = (fun ?context:_ _ -> ());
      shadow = (fun ?context:_ _ _ y -> y);
      hook = (fun ?context:_ _ -> function _ -> .) }

let run_checker f =
  Checker.run f
    { resolve =
        (fun p ->
           match S.resolve p with
           | None -> not_in_scope p
           | Some (data, tag) -> Used.use tag; data) }

let run f h =
  try run_used @@ fun () -> run_scope @@ fun () -> run_checker f with
  | [%effect? Load p, k] -> Algaeff.Fun.Deep.finally k @@ fun () -> h.load p
  | [%effect? Preload p, k] -> Algaeff.Fun.Deep.finally k @@ fun () -> h.preload p
  | [%effect? WarnUnused i, k] -> Algaeff.Fun.Deep.finally k @@ fun () -> h.warn_unused i
