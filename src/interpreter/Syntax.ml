include Elaborator.Syntax

type empty = |
type modifier = empty Yuujinchou.Language.t

type cmd = cmd_ Asai.Span.located
and cmd_ =
  | Axiom of {name : bound_name; tp : t}
  | Def of {name : bound_name; tm : t}
  | Import of {unit_path : Bantorra.Manager.path; modifier : modifier}
  | Section of {prefix : Yuujinchou.Trie.path; block : section}
  | Quit

and section = section_ Asai.Span.located
and section_ = cmd list

type prog = section
