open Earley_core

let lexing_position i c =
  Lexing.{
    pos_fname = Input.filename i;
    pos_lnum = Input.line_num i;
    pos_bol = Input.line_offset i;
    pos_cnum = Input.line_offset i + c;
    (* XXX: How to do UTF-8 here? Should be possible with Input.utf8_col_num. *)
  }

let locate i1 c1 i2 c2 =
  Asai.Span.of_lex_pos (lexing_position i1 c1) (lexing_position i2 c2)

let located p =
  p |> Earley.apply_position @@ fun i1 c1 i2 c2 x ->
  Asai.Loc.{
    value = x;
    span = locate i1 c1 i2 c2;
  }
