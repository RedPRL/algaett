type info =
  | Imported of Bantorra.Manager.path Checker.Syntax.node
  | Local of Yuujinchou.Trie.path Checker.Syntax.node

module Internal =
struct
  module U = Algaeff.UniqueID.Make (struct type elt = info end)
  module IDSet = Set.Make (U.ID)
  module S = Algaeff.State.Make (struct type state = IDSet.t end)
end

open Internal

type _ Effect.t += WarnUnused : info -> unit Effect.t

type handler = { warn_unused : info -> unit }

let perform : handler = { warn_unused = fun info -> Effect.perform (WarnUnused info) }

type id = U.id

let compare_id = U.ID.compare

let new_ u = let id = U.register u in S.modify (IDSet.add id); id

let use id = S.modify (IDSet.remove id)

let run f h =
  U.run @@ fun () -> S.run ~init:IDSet.empty @@ fun () ->
  let ans = f () in
  (* we are not using Fun.protect because exceptions should skip the warnings *)
  Seq.iter h.warn_unused @@ Seq.map U.retrieve @@ IDSet.to_seq (S.get());
  ans
