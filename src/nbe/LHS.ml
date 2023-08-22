open Bwd
open Bwd.Infix

type frame =
  | Ap of Domain.cell
  | Fst
  | Snd

type t = (Yuujinchou.Trie.path * frame bwd) option

let unknown = None
let head p = Some (p, Emp)

let app h cell =
  h |> Option.map @@ fun (p, sp) ->
  p, sp <: Ap cell

let fst =
  Option.map @@ fun (p, sp) ->
  p, sp <: Fst

let snd =
  Option.map @@ fun (p, sp) ->
  p, sp <: Snd
