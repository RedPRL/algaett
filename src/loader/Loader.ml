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

let load input mode =
  let display = match mode with
    | Some ("--debug" | "-d") -> Terminal.display ~display_traces:true
    | Some ("--interactive" | "-i") -> Terminal.interactive_trace
    | Some _ -> failwith "unknown arg"
    | None -> Terminal.display ~display_traces:false
  in 
  match input with
  | `File filename ->
    let prog = Parser.parse_file filename in
    Error.Logger.run ~emit:display ~fatal:display @@ fun () ->
    run_interpreter @@ fun () ->
    Interpreter.execute prog

