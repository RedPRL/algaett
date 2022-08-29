let () = Printexc.record_backtrace true

let () = Loader.load (`File Sys.argv.(1)) (if Array.length Sys.argv > 2 then Some Sys.argv.(2) else None)
