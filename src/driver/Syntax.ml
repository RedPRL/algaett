include Checker.Syntax

type cmd = cmd_ node
and cmd_ =
  | Axiom of {name : ident; tp : t}
  | Def of {name : ident; tm : t; tp : t}
  | Import of {unit_path : Bantorra.Manager.unitpath; modifier : Scope.modifier}
  | Section of {prefix : Yuujinchou.Trie.path; block : section}
  | Quit

and section = section_ node
and section_ = cmd list

type prog = section
