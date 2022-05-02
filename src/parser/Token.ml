[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

open KeywordClass

let keyword = Earley.alternatives [AsciiToken.keyword; EmojiToken.keyword]
let name = Earley.alternatives [AsciiToken.name; EmojiToken.name]
let pos_num = Earley.alternatives [AsciiToken.pos_num; EmojiToken.pos_num]
let num = Earley.alternatives [AsciiToken.num; EmojiToken.num]

type tag = {tag : string list}
type check_tag = {check_tag : string list -> bool}
let underscore = keyword |> Earley.apply @@ function Underscore -> () | _ -> Earley.give_up ()
let term_field = keyword |> Earley.apply @@ function TermField f -> f | _ -> Earley.give_up ()
let term_constant = keyword |> Earley.apply @@ function TermConstant t -> t | _ -> Earley.give_up ()
let term_vir_tp = keyword |> Earley.apply @@ function TermVirtualType t -> t | _ -> Earley.give_up ()
let def = keyword |> Earley.apply @@ function CmdDef -> () | _ -> Earley.give_up ()
let axiom = keyword |> Earley.apply @@ function CmdAxiom -> () | _ -> Earley.give_up ()
let quit = keyword |> Earley.apply @@ function CmdQuit -> () | _ -> Earley.give_up ()
let import = keyword |> Earley.apply @@ function CmdImport -> () | _ -> Earley.give_up ()
let section_start = keyword |> Earley.apply @@ function CmdSectionStart {tag} -> {tag} | _ -> Earley.give_up ()
let section_end = keyword |> Earley.apply @@ function CmdSectionEnd {check_tag} -> {check_tag} | _ -> Earley.give_up ()

let at = Earley.greedy @@ parser {STR(Emoji.bullseye) | "@"} -> ()
let colon = Earley.greedy @@ parser ":" -> ()
let comma = Earley.greedy @@ parser "," -> ()
let dollar = Earley.greedy @@ parser {STR(Emoji.back_arrow) | STR(Emoji.dollar_banknote) | "$"} -> ()
let equal = Earley.greedy @@ parser { STR(Emoji.heavy_equals_sign) | "=" } -> ()
let maps_to = Earley.greedy @@ parser {STR(Emoji.left_arrow_curving_right) | "=>"} -> ()
let multiply = Earley.greedy @@ parser { STR(Emoji.multiply) | "*" } -> ()
let plus = Earley.greedy @@ parser { STR(Emoji.plus) | "+" } -> ()
let right_arrow = Earley.greedy @@ parser {STR(Emoji.right_arrow) | "→" | "->"} -> ()
let semi = Earley.greedy @@ parser {";"} -> ()
let semisemi = Earley.greedy @@ parser {";;"} -> ()
let times = Earley.greedy @@ parser {STR(Emoji.multiply) | STR(Emoji.keycap_asterisk) | "×" | "*"} -> ()
let up = Earley.greedy @@ parser {STR(Emoji.up_arrow) | "^"} -> ()
let assign = Earley.greedy @@ parser
    { ":="
    | "≔"
    | STR(Emoji.backhand_index_pointing_right)
    | STR(Emoji.backhand_index_pointing_right_light_skin_tone)
    | STR(Emoji.backhand_index_pointing_right_medium_light_skin_tone)
    | STR(Emoji.backhand_index_pointing_right_medium_skin_tone)
    | STR(Emoji.backhand_index_pointing_right_medium_dark_skin_tone)
    | STR(Emoji.backhand_index_pointing_right_dark_skin_tone)
    } -> ()
