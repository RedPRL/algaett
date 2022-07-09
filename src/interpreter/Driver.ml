module CS = Syntax
module UE = UnitEffect

exception Quit (* local *)

let include_singleton ?loc name data =
  match name with
  | None -> ()
  | Some p -> UE.include_singleton ?loc (p, data)

let rec execute_decl {CS.node = decl; CS.loc = loc} =
  match decl with
  | CS.Axiom {name; tp} ->
    let tp = NbE.eval_top @@ UE.reraise_elaborator @@ Elaborator.check_tp_top NbE.LHS.unknown tp in
    include_singleton ?loc (name : CS.bound_name) @@ Axiom {tp}
  | CS.Def {name; tm} ->
    let lhs = Option.fold ~none:NbE.LHS.unknown ~some:NbE.LHS.head name in
    let tm, tp = UE.reraise_elaborator @@ Elaborator.infer_top lhs tm in
    include_singleton ?loc name @@ Def {tm = lazy begin NbE.eval_top tm end; tp}
  | CS.Import {unit_path; modifier} ->
    UE.import ?loc unit_path modifier
  | CS.Section {prefix; block} ->
    UE.section prefix @@ fun () -> execute_section block
  | CS.Quit -> raise Quit

and execute_section sec =
  List.iter execute_decl sec.CS.node

let execute prog =
  UnitEffect.trap @@ fun () -> try execute_section prog with Quit -> ()
