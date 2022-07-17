open Asai
module CS = Syntax
module UE = UnitEffect

exception Quit (* local *)

let include_singleton span name data =
  match name with
  | None -> ()
  | Some p -> UE.include_singleton span (p, data)

let rec execute_decl {Loc.value = decl; Loc.span } =
  match decl with
  | CS.Axiom {name; tp} ->
    let tp = NbE.eval_top @@ Elaborator.check_tp_top NbE.LHS.unknown tp in
    include_singleton span (name : CS.bound_name) @@ Axiom {tp}
  | CS.Def {name; tm} ->
    let lhs = Option.fold ~none:NbE.LHS.unknown ~some:NbE.LHS.head name in
    let tm, tp = Elaborator.infer_top lhs tm in
    include_singleton span name @@ Def {tm = lazy begin NbE.eval_top tm end; tp}
  | CS.DefChk {name ; tp ; tm} ->
    let lhs = Option.fold ~none:NbE.LHS.unknown ~some:NbE.LHS.head name in
    let tp = NbE.eval_top @@ Elaborator.check_tp_top NbE.LHS.unknown tp in
    let tm = Elaborator.check_top lhs tm ~tp in
    include_singleton span name @@ Def {tm = lazy begin NbE.eval_top tm end; tp}
  | CS.Import {unit_path; modifier} ->
    UE.import span unit_path modifier
  | CS.Section {prefix; block} ->
    UE.section prefix @@ fun () -> execute_section block
  | CS.Quit -> raise Quit

and execute_section sec =
  List.iter execute_decl sec.Loc.value

let execute prog =
  try execute_section prog with Quit -> ()
