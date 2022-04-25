open Algaeff.StdlibShim

type data = Def of {tm: NbE.Domain.t; tp: NbE.Domain.t}
type empty = |
module S = Yuujinchou.Scope.Make (struct type nonrec data = data type hook = empty end)

let run f =
  let open Effect.Deep in
  try_with S.run f
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Act.BindingNotFound _ -> Option.some @@
            fun (k : (a, _) continuation) -> continue k ()
          | S.Act.Shadowing (_, _, _, new_data) -> Option.some @@
            fun (k : (a, _) continuation) -> continue k new_data
          | S.Act.Hook (_, _, _, _) -> .
          | _ -> None }

let resolve = S.resolve
let include_singleton = S.include_singleton
let section = S.section
