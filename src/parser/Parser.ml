open Earley_core

let _parse_file f = Earley.parse_file Cmd.prog Blanks.default f

let _ = Earley.parse_string Cmd.prog Blanks.default Sys.argv.(1)
