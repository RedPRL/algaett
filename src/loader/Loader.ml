module Terminal = Asai.Tty.Make(Error.Code)

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
    Error.run ~emit:Terminal.display ~fatal:Terminal.display @@ fun () ->
    let prog = Parser.parse_file filepath in
    run_interpreter @@ fun () -> Interpreter.execute prog
