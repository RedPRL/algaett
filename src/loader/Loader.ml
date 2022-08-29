module InterpreterHandler : Interpreter.Handler =
struct
  let load _ = raise Not_found
  let preload _ = ()
  let warn_unused _ = ()
end

module HandleInterpreter = Interpreter.Run (InterpreterHandler)

let run_interpreter =
  HandleInterpreter.run

module Terminal = Asai_unix.Make(Error.Logger.Code)

let load =
  function
  | `File filename ->
    let prog = Parser.parse_file filename in
    Error.Logger.run ~emit:Terminal.display ~fatal:Terminal.display @@ fun () ->
    run_interpreter @@ fun () ->
    Interpreter.execute prog
