open Asai
open Error
module Terminal = Asai_unix.Make(Doctor.ErrorCode)

module InterpreterHandler : Interpreter.Handler =
struct
  let load _ = raise Not_found
  let preload _ = ()
  let warn_unused _ = ()
end

module HandleInterpreter = Interpreter.Run (InterpreterHandler)

let run_interpreter =
  HandleInterpreter.run

let load =
  function
  | `File filepath ->
    let span = Span.file_start filepath in
    Doctor.run_display ~span ~display:Terminal.display @@ fun () ->
    let prog = Parser.parse_file filepath in
    let contents = 
      let ch = open_in filepath in
      let str = really_input_string ch (in_channel_length ch) in
      close_in ch;
      str
    in
    Error.Doctor.load_file ~filepath contents;
    run_interpreter @@ fun () ->
    Interpreter.execute prog
