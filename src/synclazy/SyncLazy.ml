type 'a t = 'a Lazy.t * Mutex.t

let[@inline] from_lazy v = v, Mutex.create ()
let[@inline] from_val v = from_lazy (Lazy.from_val v)

let force (v, m) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () ->
  Lazy.force v

let map f (v, m) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () ->
  Lazy.map f v, m
