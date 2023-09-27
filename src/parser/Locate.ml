open Earley_core

let lexing_position i c =
  Lexing.{
    pos_fname = Input.filename i;
    pos_lnum = Input.line_num i;
    pos_bol = Input.line_offset i;
    pos_cnum = Input.line_offset i + c;
    (* This should be BYTE-oriented! *)
  }

let located p =
  p |> Earley.apply_position @@ fun i1 c1 i2 c2 x ->
  Asai.Span.locate_lex (lexing_position i1 c1, lexing_position i2 c2) x
