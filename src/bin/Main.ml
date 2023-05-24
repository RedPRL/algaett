
let () =
  Printexc.record_backtrace true;
  Array.iteri (fun i f -> if i > 0 then Loader.load (`File f)) Sys.argv
