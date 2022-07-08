module InterpreterHandler : Interpreter.Handler =
struct
  let load _ = raise Not_found
  let preload _ = ()
  let warn_unused _ = ()
end

module RunInterpreter = Interpreter.Run (InterpreterHandler)

let run_interpreter =
  RunInterpreter.run

let load =
  function
  | `File filename ->
    let prog = Parser.parse_file filename in
    Result.get_ok @@ run_interpreter @@ fun () ->
    Interpreter.execute prog
