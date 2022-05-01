open Earley_core

let parse_file f = Earley.parse_file Cmd.prog Blanks.default f
let parse_repl ?filename () = Earley.parse_channel ?filename Cmd.repl Blanks.default In_channel.stdin
