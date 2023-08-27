open Earley_core

let lexing_position i c =
  Asai.Span.of_lex_position @@
  Lexing.{
    pos_fname = Input.filename i;
    pos_lnum = Input.line_num i;
    pos_bol = Input.line_offset i;
    pos_cnum = Input.line_offset i + c;
    (* This should be BYTE-oriented! *)
  }

let locate i1 c1 i2 c2 =
  Asai.Span.make (lexing_position i1 c1) (lexing_position i2 c2)

let located p =
  p |> Earley.apply_position @@ fun i1 c1 i2 c2 x ->
  Asai.Span.{
    value = x;
    loc = Some (locate i1 c1 i2 c2);
  }
