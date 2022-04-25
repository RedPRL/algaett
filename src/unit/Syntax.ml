include Decl.Syntax

type cmd = cmd_ node
and cmd_ =
  | Axiom of {name : ident; tp : t}
  | Def of {name : ident; tm : t; tp : t}
  | Section of {prefix : Yuujinchou.Trie.path; block : cmd list}
  | Quit

and block
