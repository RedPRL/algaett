type span =
  {start : Lexing.position;
   stop : Lexing.position}

type name = Yuujinchou.Trie.path
type bound_name = name option

type 'a node = {node : 'a; info : span option}

type t = t_ node
and t_ =
  | Ann of {tm : t; tp : t}
  | Var of name
  | Pi of t * bound_name * t
  | Lam of bound_name * t
  | App of t * t
  | Sigma of t * bound_name * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of t
  | VirPi of t * bound_name * t
  | TpULvl
  | ULvlTop
  | ULvlShifted of t * int list
