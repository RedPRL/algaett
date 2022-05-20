type info = Bantorra.Manager.path Checker.Syntax.node

type id

val new_ : info -> id
val use : id -> unit

type handler = { warn_unused : info -> unit }
val run : (unit -> 'a) -> handler -> 'a
val reperform : handler
