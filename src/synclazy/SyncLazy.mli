type !+'a t
val from_lazy : 'a Lazy.t -> 'a t
val from_val : 'a -> 'a t
val force : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
