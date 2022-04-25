type span =
  {start : Lexing.position;
   stop : Lexing.position}

type ident = Yuujinchou.Trie.path option

type 'a node = {node : 'a; info : span option}

type t = t_ node
and t_ =
  | Ann of {tm : t; tp : t}
  | Var of Yuujinchou.Trie.path
  | Pi of t * ident * t
  | Lam of ident * t
  | App of t * t
  | Sigma of t * ident * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of t
  | VirPi of t * ident * t
  | TpULvl
  | ULvlTop
  | ULvlShifted of t * int list
