module CS = Syntax
module UE = UnitEffect

exception Quit (* local *)

let include_singleton name data =
  match name with
  | None -> ()
  | Some p -> UE.include_singleton (p, data)

let rec execute_decl decl =
  match decl.CS.node with
  | CS.Axiom {name; tp} ->
    let tp = NbE.eval_top @@ UE.reraise_checker @@ Checker.check_tp_top tp in
    include_singleton name @@ Axiom {tp}
  | CS.Def {name; tm} ->
    let tm, tp = UE.reraise_checker @@ Checker.infer_top tm in (* we want to type check the term now *)
    include_singleton name @@ Def {tm = lazy begin NbE.eval_top tm end; tp}
  | CS.Import {unit_path; modifier} ->
    UE.import {CS.node = unit_path; CS.info = decl.CS.info} modifier
  | CS.Section {prefix; block} ->
    UE.section prefix @@ fun () -> execute_section block
  | CS.Quit -> raise Quit

and execute_section sec =
  List.iter execute_decl sec.CS.node

let execute prog =
  UnitEffect.trap @@ fun () -> try execute_section prog with Quit -> ()
