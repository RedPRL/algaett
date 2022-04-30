[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core
open EmojiShim

open KeywordClass

let keyword = Earley.alternatives [AsciiToken.keyword; EmojiToken.keyword]
let name = Earley.alternatives [AsciiToken.name; EmojiToken.name]
let num = Earley.alternatives [AsciiToken.num; EmojiToken.num]

type tag = {tag : string list}
type check_tag = {check_tag : string list -> bool}
let underscore = keyword |> Earley.apply @@ function Underscore -> () | _ -> Earley.give_up ()
let term_field = keyword |> Earley.apply @@ function TermField f -> f | _ -> Earley.give_up ()
let term_fun1 = keyword |> Earley.apply @@ function TermFun1 t -> t | _ -> Earley.give_up ()
let term_vir_tp = keyword |> Earley.apply @@ function TermVirtualType t -> t | _ -> Earley.give_up ()
let def = keyword |> Earley.apply @@ function CmdDef -> () | _ -> Earley.give_up ()
let axiom = keyword |> Earley.apply @@ function CmdAxiom -> () | _ -> Earley.give_up ()
let quit = keyword |> Earley.apply @@ function CmdQuit -> () | _ -> Earley.give_up ()
let import = keyword |> Earley.apply @@ function CmdImport -> () | _ -> Earley.give_up ()
let section_start = keyword |> Earley.apply @@ function CmdSectionStart {tag} -> {tag} | _ -> Earley.give_up ()
let section_end = keyword |> Earley.apply @@ function CmdSectionEnd {check_tag} -> {check_tag} | _ -> Earley.give_up ()

let parser up = {"^" | STR(Emoji.up_arrow) } -> ()
let parser at = {"@" | STR(Emoji.direct_hit) } -> ()
let parser colon = ":" -> ()
let parser comma = "," -> ()
let parser right_arrow = {STR(Emoji.right_arrow) | "→" | "->"} -> ()
let parser maps_to = {STR(Emoji.left_arrow_curving_right) | "=>"} -> ()
let parser times = {STR(Emoji.heavy_multiplication_x) | STR(Emoji.keycap_asterisk) | "×" | "*"} -> ()

let parser assign =
  { ":="
  | "≔"
  | STR(Emoji.backhand_index_pointing_right)
  | STR(Emoji.backhand_index_pointing_right_light_skin_tone)
  | STR(Emoji.backhand_index_pointing_right_medium_light_skin_tone)
  | STR(Emoji.backhand_index_pointing_right_medium_skin_tone)
  | STR(Emoji.backhand_index_pointing_right_medium_dark_skin_tone)
  | STR(Emoji.backhand_index_pointing_right_dark_skin_tone)
  } -> ()
