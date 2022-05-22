let () = Printexc.record_backtrace true

let () = Loader.load (`File Sys.argv.(1))
