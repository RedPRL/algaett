let run_interpreter f =
  let open Interpreter in
  Interpreter.run f
    { load = (fun _ -> raise Not_found);
      preload = (fun _ -> ());
      warn_unused = (fun _ -> ()) }

let load =
  function
  | `File filename ->
    let prog = Parser.parse_file filename in
    Result.get_ok @@ run_interpreter @@ fun () -> Interpreter.execute prog
