open Earley_core

let lexing_position i c =
  Lexing.{
    pos_fname = Input.filename i;
    pos_lnum = Input.line_num i;
    pos_bol = Input.line_offset i;
    pos_cnum = Input.utf8_col_num i (Input.line_offset i + c);
    (* XXX: is it expensive to call utf8_col_num all the times? *)
  }

let locate i1 c1 i2 c2 =
  Checker.Syntax.{
    start = lexing_position i1 c1;
    stop = lexing_position i2 c2;
  }

let located p =
  p |> Earley.apply_position @@ fun i1 c1 i2 c2 x ->
  Checker.Syntax.{
    node = x;
    info = Some (locate i1 c1 i2 c2);
  }
