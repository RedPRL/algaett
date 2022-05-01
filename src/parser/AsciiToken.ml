[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

let parser sep = "." -> ()
let parser raw_seg_ = ''[a-zA-Z_][0-9a-zA-Z+_-]*''
let raw_seg = Earley.(no_blank_layout @@ greedy @@ raw_seg_)

let keywords =
  let open KeywordClass in
  let module S = Checker.Syntax in
  Hashtbl.of_seq @@ List.to_seq [
    "_", Underscore;

    (* term *)
    "fst", TermField (fun tm -> S.Fst tm);
    "snd", TermField (fun tm -> S.Snd tm);
    "univ", TermConstant (fun ~shift -> S.Univ shift);

    (* cmd *)
    "def", CmdDef;
    "axiom", CmdAxiom;
    "quit", CmdQuit;
    "import", CmdImport;
    "section", CmdSectionStart {tag = ["section"]};
    "end", CmdSectionEnd {check_tag = fun t -> t = ["section"]};
  ]

let keyword = raw_seg |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | Some k -> k
  | None -> Earley.give_up ()
let seg = raw_seg |> Earley.apply @@ fun token ->
  match Hashtbl.mem keywords token with
  | true -> Earley.give_up ()
  | false -> token
let name = Earley.list1 seg sep

let parser pos_num =
  | i:''[0-9]+'' -> match int_of_string_opt i with Some i -> i | _ -> Earley.give_up ()
let parser num_ =
  | "+"? i:pos_num -> i
  | "-" i:pos_num -> -i
let num = Earley.no_blank_layout num_
