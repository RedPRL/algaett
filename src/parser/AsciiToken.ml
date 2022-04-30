[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

let parser ascii_sep = "." -> ()
let parser ascii_seg = ''[a-zA-Z_][0-9a-zA-Z+_-]*''

let keywords =
  let open KeywordClass in
  let module S = Checker.Syntax in
  Hashtbl.of_seq @@ List.to_seq [
    ["_"], Underscore;

    (* term *)
    ["fst"], TermField (fun tm -> S.Fst tm);
    ["snd"], TermField (fun tm -> S.Snd tm);
    ["univ"], TermFun1 (fun tm -> S.Univ tm);
    ["lvl"], TermVirtualType S.TpULvl;

    (* cmd *)
    ["def"], CmdDef;
    ["axiom"], CmdAxiom;
    ["quit"], CmdQuit;
    ["import"], CmdImport;
    ["section"], CmdSectionStart {tag = ["section"]};
    ["end"], CmdSectionEnd {check_tag = fun t -> t = ["section"]};
  ]

let raw = Earley.(no_blank_layout @@ list1 (greedy ascii_seg) ascii_sep)
let keyword = raw |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | Some k -> k
  | None -> Earley.give_up ()
let name = raw |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | None -> token
  | Some _ -> Earley.give_up ()

let parser pos_num =
  | i:''[0-9]+'' -> match int_of_string_opt i with Some i -> i | _ -> Earley.give_up ()
let parser num_ =
  | "+"? i:pos_num -> i
  | "-" i:pos_num -> -i
let num = Earley.no_blank_layout num_
