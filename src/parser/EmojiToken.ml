[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

module StringSet = Set.Make (String)

let keycap_asterisk = "\x2a\xef\xb8\x8f\xe2\x83\xa3"
let ladder = "\xf0\x9f\xaa\x9c"

module EmojiSet =
struct
  let all = StringSet.of_list (keycap_asterisk :: ladder :: Emoji.all_emojis)

  (* XXX dependency: the correctness of the keyword table in Token highly depends on
     what symbols have been excluded from raw names. *)
  let question_mark = StringSet.of_list Emoji.[
      exclamation_question_mark;
      question_mark;
      white_question_mark;
    ]
  let sep = StringSet.of_list Emoji.[
      small_orange_diamond;
      small_blue_diamond;
    ]
  let other_symbol = StringSet.of_list Emoji.[
      keycap_asterisk; (* Keycap Asterisk *)
      right_arrow;
      left_arrow_curving_right;
      backhand_index_pointing_right;
      backhand_index_pointing_right_light_skin_tone;
      backhand_index_pointing_right_medium_light_skin_tone;
      backhand_index_pointing_right_medium_skin_tone;
      backhand_index_pointing_right_medium_dark_skin_tone;
      backhand_index_pointing_right_dark_skin_tone;
      heavy_multiplication_x;
      heavy_plus_sign;
    ]

  let symbol = List.fold_left StringSet.union StringSet.empty [question_mark; sep; other_symbol]
  let alnum = StringSet.diff all symbol

  let num = StringSet.of_list Emoji.[
      keycap_0;
      keycap_1;
      keycap_2;
      keycap_3;
      keycap_4;
      keycap_5;
      keycap_6;
      keycap_7;
      keycap_8;
      keycap_9;
      keycap_10;
      hundred_points;
    ]
  let alpha = StringSet.diff alnum num
end

(* to_rev_seq is used so that skin tones will definitely be consumed *)
let of_set s = Earley.(no_blank_layout @@ alternatives @@ List.map (fun s -> greedy (string s s)) @@ List.of_seq @@ StringSet.to_rev_seq s)
let parser emoji_sep = (of_set EmojiSet.sep) -> ()
let emoji_alnum = of_set EmojiSet.alnum
let emoji_alpha = of_set EmojiSet.alpha
let parser emoji_seg_ = x:emoji_alpha - xs:emoji_alnum* -> String.concat "" (x :: xs)
let emoji_seg = Earley.no_blank_layout emoji_seg_

let keywords =
  let open KeywordClass in
  let module S = Checker.Syntax in
  Hashtbl.of_seq @@ List.to_seq [
    [Emoji.(keycap_ ^ keycap_1)], TermField (fun tm -> S.Fst tm);
    [Emoji.(keycap_ ^ keycap_2)], TermField (fun tm -> S.Snd tm);
    [Emoji.milky_way], TermFun1 (fun tm -> S.Univ tm);
    [ladder], TermVirtualType S.TpULvl;
    [Emoji.level_slider], TermVirtualType S.TpULvl;

    [Emoji.pushpin], CmdDef;
    [Emoji.round_pushpin], CmdDef;
    [Emoji.folded_hands], CmdAxiom;
    [Emoji.folded_hands_light_skin_tone], CmdAxiom;
    [Emoji.folded_hands_medium_light_skin_tone], CmdAxiom;
    [Emoji.folded_hands_medium_skin_tone], CmdAxiom;
    [Emoji.folded_hands_medium_dark_skin_tone], CmdAxiom;
    [Emoji.folded_hands_dark_skin_tone], CmdAxiom;
    [Emoji.stop_sign], CmdQuit;
    [Emoji.inbox_tray], CmdImport;
    [Emoji.umbrella], CmdSectionStart {tag = [Emoji.umbrella]};
    [Emoji.closed_umbrella], CmdSectionEnd {check_tag = fun t -> t = [Emoji.umbrella]};
  ]

let raw = Earley.(no_blank_layout @@ list1 (greedy emoji_seg) emoji_sep)
let keyword = raw |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | Some k -> k
  | None -> Earley.give_up ()
let name = raw |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | None -> token
  | Some _ -> Earley.give_up ()
