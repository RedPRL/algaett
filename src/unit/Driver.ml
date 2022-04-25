module CS = Syntax

exception Quit

let include_singleton name data =
  match name with
  | None -> ()
  | Some p -> Scope.include_singleton (p, data)

let rec execute decl =
  match decl.CS.node with
  | CS.Axiom {name; tp} ->
    let tp = NbE.eval_top @@ Decl.check_top tp ~tp:NbE.Domain.univ_top in
    include_singleton name @@ Axiom {tp}
  | CS.Def {name; tm; tp} ->
    let tp = NbE.eval_top @@ Decl.check_top tp ~tp:NbE.Domain.univ_top in
    let tm = Decl.check_top tm ~tp in (* we want to type check the term now *)
    include_singleton name @@ Def {tm = lazy begin NbE.eval_top tm end; tp}
  | CS.Section {prefix; block} ->
    Scope.section prefix @@ fun () -> List.iter execute block
  | CS.Quit -> raise Quit
