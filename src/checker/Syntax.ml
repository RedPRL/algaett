type span =
  {start : Lexing.position;
   stop : Lexing.position}
let dump_position fmt Lexing.{pos_fname; pos_lnum; pos_bol; pos_cnum} =
  Format.fprintf fmt "@[<2>{ pos_fname = \"%s\";@ pos_lnum = %i;@ pos_bol = %i;@ pos_cnum = %i; }@]"
    (String.escaped pos_fname) pos_lnum pos_bol pos_cnum
let dump_span fmt {start; stop} =
  Format.fprintf fmt "@[<2>{ start = @[%a@];@ stop = @[%a@]; }@]" dump_position start dump_position stop

type 'a node = {node : 'a; info : span option}
let dump_node dump fmt {node; info} =
  match info with
  | None -> dump fmt node
  | Some info ->
    Format.fprintf fmt "@[<2>{ node = @[%a@];@ info = @[%a@]; }@]" dump node dump_span info

type name = Yuujinchou.Trie.path
let dump_name fmt n =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ".") Format.pp_print_string) n

type bound_name = name option
let dump_bound_name fmt =
  function
  | None -> Format.pp_print_string fmt "_"
  | Some n -> dump_name fmt n

type t = t_ node
and t_ =
  | Ann of {tm : t; tp : t}
  | Var of name * int list option
  | Pi of t * bound_name * t
  | Lam of bound_name * t
  | App of t * t
  | Sigma of t * bound_name * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of int list option
  | VirPi of t * bound_name * t

(** ugly printer for the arguments of ULvlShifted *)
let dump_shift fmt l =
  Format.fprintf fmt "@[%a...@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";") Format.pp_print_int) l

let rec dump fmt =
  dump_node dump_ fmt
and dump_ fmt =
  function
  | Ann {tm; tp} ->
    Format.fprintf fmt "@[<6>Ann { tm = @[%a@];@ stop = @[%a@]; }@]" dump tm dump tp
  | Var (name, None) ->
    Format.fprintf fmt "Var @[%a@]" dump_name name
  | Var (name, Some s) ->
    Format.fprintf fmt "@[<6>Var ( @[%a@],@ @[%a@] )" dump_name name dump_shift s
  | Pi (base, bound, fam) ->
    Format.fprintf fmt "@[<5>Pi ( @[%a@],@ @[%a@],@ @[%a@] )@]" dump base dump_bound_name bound dump fam
  | Lam (bound, tm) ->
    Format.fprintf fmt "@[<6>Lam ( @[%a@],@ @[%a@] )@]" dump_bound_name bound dump tm
  | App (tm1, tm2) ->
    Format.fprintf fmt "@[<6>App ( @[%a@],@ @[%a@] )@]" dump tm1 dump tm2
  | Sigma (base, bound, fam) ->
    Format.fprintf fmt "@[<8>Sigma ( @[%a@],@ @[%a@],@ @[%a@] )@]" dump base dump_bound_name bound dump fam
  | Pair (tm1, tm2) ->
    Format.fprintf fmt "@[<7>Pair ( @[%a@],@ @[%a@] )@]" dump tm1 dump tm2
  | Fst tm ->
    Format.fprintf fmt "Fst @[%a@]" dump tm
  | Snd tm ->
    Format.fprintf fmt "Snd @[%a@]" dump tm
  | Univ None ->
    Format.pp_print_string fmt "Univ"
  | Univ (Some s) ->
    Format.fprintf fmt "Univ @[%a@]" dump_shift s
  | VirPi (base, bound, fam) ->
    Format.fprintf fmt "@[<8>VirPi ( @[%a@],@ @[%a@],@ @[%a@] )@]" dump base dump_bound_name bound dump fam
