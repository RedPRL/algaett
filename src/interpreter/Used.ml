type info = Bantorra.Manager.path Checker.Syntax.node

module Internal =
struct
  module A = Algaeff.AutoIncrement.Make (struct type row = info end)
  module IntSet = Set.Make (Int)
  module S = Algaeff.State.Make (struct type state = IntSet.t end)
end

open Internal

exception%effect WarnUnused : info -> unit
type handler = { warn_unused : info -> unit }
let reperform : handler = { warn_unused = fun info -> Effect.perform (WarnUnused info) }

type id = A.id
let new_ u = let id = A.insert u in S.modify (IntSet.add id); id
let use id = S.modify (IntSet.remove id)

let run f h =
  A.run @@ fun () -> S.run ~init:IntSet.empty @@ fun () ->
  let ans = f () in
  (* we are not using Fun.protect because exceptions should skip the warnings *)
  Seq.iter h.warn_unused @@ Seq.map A.select @@ IntSet.to_seq (S.get());
  ans
