open Algaeff.StdlibShim

exception NotInScope
type _ Effect.t +=
  | ResolveUnit : Bantorra.Manager.unitpath -> Checker.resolve_data Yuujinchou.Trie.t Effect.t
  | UnusedImports : Bantorra.Manager.unitpath list -> unit Effect.t

type empty = |
type modifier = empty Yuujinchou.Modifier.t

type resolve_data =
  { source : Bantorra.Manager.unitpath option
  ; data : Checker.resolve_data
  }

module Usage =
struct
  module S = Set.Make (struct type t = Bantorra.Manager.unitpath let compare = compare end)
  type state = {imported: S.t; used: S.t}
  module E = Algaeff.State.Make (struct type nonrec state = state end)
  let import u = E.modify (fun st -> {st with imported = S.add u st.imported})
  let use u = Option.iter (fun u -> E.modify (fun st -> {st with used = S.add u st.used})) u
  let unused () = let st = E.get() in S.diff st.used st.imported
  let run f = E.run ~init:{imported = S.empty; used = S.empty} f
end

module S = Yuujinchou.Scope.Make (struct type data = resolve_data type hook = empty end)

let include_singleton (p, data) = S.include_singleton (p, {data; source = None})
let section = S.section
let get_export () = Yuujinchou.Trie.mapi (fun ~path:_ d -> d.data) @@ S.get_export()
let import source m =
  Usage.import source;
  let u = S.Act.exec m @@
    Yuujinchou.Trie.mapi (fun ~path:_ data -> {source = Some source; data}) @@
    Effect.perform (ResolveUnit source)
  in
  S.import_subtree ([], u)

let handle_resolve f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Checker.Resolve p -> Option.some @@
            fun (k : (a, _) continuation) ->
            Algaeff.Fun.Deep.finally k (fun () ->
                match S.resolve p with
                | None -> raise NotInScope
                | Some data -> Usage.use data.source; data.data)
          | _ -> None }

let run f =
  let f () =
    Usage.run @@ fun () -> S.run ~prefix:Emp @@ fun () ->
    let ans = handle_resolve f in
    Yuujinchou.Trie.iteri (fun ~path:_ d -> Usage.use d.source) @@ S.get_export();
    let unused_imports = Usage.unused () in
    if not (Usage.S.is_empty unused_imports) then
      Effect.perform (UnusedImports (List.of_seq (Usage.S.to_seq unused_imports)));
    ans
  in
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Act.BindingNotFound _ -> Option.some @@
            fun (k : (a, _) continuation) -> continue k ()
          | S.Act.Shadowing (_, _, _, new_data) -> Option.some @@
            fun (k : (a, _) continuation) -> continue k new_data
          | S.Act.Hook (_, _, _, _) -> .
          | _ -> None }
