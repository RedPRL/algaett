let () = Printexc.record_backtrace true

let () = 
  exit @@ Loader.load (`File Sys.argv.(1))