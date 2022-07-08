module InterpreterHandler : Interpreter.Handler =
struct
  let load _ = raise Not_found
  let preload _ = ()
  let warn_unused _ = ()
end

module HandleInterpreter = Interpreter.Handle (InterpreterHandler)

let run_interpreter =
  HandleInterpreter.run

let load =
  function
  | `File filename ->
    let prog = Parser.parse_file filename in
    Result.get_ok @@ run_interpreter @@ fun () ->
    Interpreter.execute prog
