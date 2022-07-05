open Bwd
open BwdNotation

type cell = RefineEffect.cell

type frame =
  | Ap of cell
  | Fst
  | Snd

type t = (Yuujinchou.Trie.path * frame bwd) option

let unknown = None
let head p = Some (p, Emp)

let app h cell =
  h |> Option.map @@ fun (p, sp) ->
  p, sp #< (Ap cell)

let fst =
  Option.map @@ fun (p, sp) ->
  p, sp #< Fst

let snd =
  Option.map @@ fun (p, sp) ->
  p, sp #< Snd
