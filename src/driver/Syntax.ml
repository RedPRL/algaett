include Checker.Syntax

type cmd = cmd_ node
and cmd_ =
  | Axiom of {name : bound_name; tp : t}
  | Def of {name : bound_name; tm : t}
  | Import of {unit_path : Bantorra.Manager.path; modifier : Scope.modifier}
  | Section of {prefix : Yuujinchou.Trie.path; block : section}
  | Quit

and section = section_ node
and section_ = cmd list

type prog = section
