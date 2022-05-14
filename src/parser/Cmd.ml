[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core
open Locate
open Token

module S = Driver.Syntax

let parser section_ = cmd*
and section = located section_
and parser cmd_ =
  | def name:(Term.bound_name) colon tp:(Term.term) assign tm:(Term.term) ->
      S.Def {name; tm = {node = S.Ann {tm; tp}; info = None}}
  | def name:(Term.bound_name) assign tm:(Term.term) ->
      S.Def {name; tm}
  | axiom name:(Term.bound_name) colon tp:(Term.term) ->
      S.Axiom {name; tp}
  | quit ->
    S.Quit
  | import u:name ->
      S.Import {unit_path = u; modifier = Yuujinchou.Language.seq []}
  | tag:section_start prefix:{EMPTY -> [] | name} block:section check:section_end ->
      if check.check_tag tag.tag then S.Section {prefix; block} else Earley.give_up ()
and cmd = located cmd_

let parser prog = section EOF
let parser repl = s:section {EOF | semisemi} -> s
