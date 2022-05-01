[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

module StringSet = Set.Make (String)

module EmojiSet =
struct
  let all = StringSet.of_list Emoji.all_emojis

  let question_mark = StringSet.of_list Emoji.[
      exclamation_question_mark;
      red_question_mark;
      white_question_mark;
    ]
  let sep = StringSet.of_list Emoji.[
      small_orange_diamond;
      small_blue_diamond;
    ]
  let other_symbol = StringSet.of_list Emoji.[
      bullseye;
      keycap_asterisk; (* Keycap Asterisk *)
      up_arrow;
      right_arrow;
      left_arrow_curving_right;
      backhand_index_pointing_right;
      backhand_index_pointing_right_light_skin_tone;
      backhand_index_pointing_right_medium_light_skin_tone;
      backhand_index_pointing_right_medium_skin_tone;
      backhand_index_pointing_right_medium_dark_skin_tone;
      backhand_index_pointing_right_dark_skin_tone;
      multiply;
      plus;
      minus;
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
let parser sep = (of_set EmojiSet.sep) -> ()
let alnum = of_set EmojiSet.alnum
let alpha = of_set EmojiSet.alpha
let parser raw_seg_ = x:alpha - xs:alnum* -> String.concat "" (x :: xs)
let raw_seg = Earley.(no_blank_layout @@ greedy @@ raw_seg_)

let keywords =
  let open KeywordClass in
  let module S = Checker.Syntax in
  Hashtbl.of_seq @@ List.to_seq [
    Emoji.(keycap_number_sign ^ keycap_1), TermField (fun tm -> S.Fst tm);
    Emoji.(keycap_number_sign ^ keycap_2), TermField (fun tm -> S.Snd tm);
    Emoji.milky_way, TermConstant (fun ~shift -> S.Univ shift);

    Emoji.pushpin, CmdDef;
    Emoji.round_pushpin, CmdDef;
    Emoji.folded_hands, CmdAxiom;
    Emoji.folded_hands_light_skin_tone, CmdAxiom;
    Emoji.folded_hands_medium_light_skin_tone, CmdAxiom;
    Emoji.folded_hands_medium_skin_tone, CmdAxiom;
    Emoji.folded_hands_medium_dark_skin_tone, CmdAxiom;
    Emoji.folded_hands_dark_skin_tone, CmdAxiom;
    Emoji.stop_sign, CmdQuit;
    Emoji.inbox_tray, CmdImport;
    Emoji.umbrella, CmdSectionStart {tag = [Emoji.umbrella]};
    Emoji.closed_umbrella, CmdSectionEnd {check_tag = fun t -> t = [Emoji.umbrella]};
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

let parser digit =
  | STR(Emoji.keycap_0) -> "0"
  | STR(Emoji.keycap_1) -> "1"
  | STR(Emoji.keycap_2) -> "2"
  | STR(Emoji.keycap_3) -> "3"
  | STR(Emoji.keycap_4) -> "4"
  | STR(Emoji.keycap_5) -> "5"
  | STR(Emoji.keycap_6) -> "6"
  | STR(Emoji.keycap_7) -> "7"
  | STR(Emoji.keycap_8) -> "8"
  | STR(Emoji.keycap_9) -> "9"
  | STR(Emoji.keycap_10) -> "10"
  | STR(Emoji.hundred_points) -> "100"
let parser pos_num =
  | ds:digit+ ->
       match int_of_string_opt (String.concat "" ds) with
       | Some i -> i
       | None -> Earley.give_up ()
let parser num_ =
  | STR(Emoji.plus)? i:pos_num -> i
  | STR(Emoji.minus) i:pos_num -> -i
let num = Earley.no_blank_layout num_
