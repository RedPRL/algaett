open Algaeff.StdlibShim

type empty = |
module S = Yuujinchou.Scope.Make (struct type nonrec data = Decl.data type hook = empty end)

exception NotInScope

let include_singleton = S.include_singleton
let section = S.section

let handle_resolve f () =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Decl.Resolve p -> Option.some @@
            fun (k : (a, _) continuation) ->
            Algaeff.Fun.Deep.finally k (fun () ->
                match S.resolve p with
                | None -> raise NotInScope
                | Some data -> data)
          | _ -> None }

let run f =
  let open Effect.Deep in
  try_with (S.run ~prefix:Emp) (handle_resolve f)
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Act.BindingNotFound _ -> Option.some @@
            fun (k : (a, _) continuation) -> continue k ()
          | S.Act.Shadowing (_, _, _, new_data) -> Option.some @@
            fun (k : (a, _) continuation) -> continue k new_data
          | S.Act.Hook (_, _, _, _) -> .
          | _ -> None }
