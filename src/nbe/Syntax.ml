type t = Data.syn =
  | Var of int
  | Axiom of Yuujinchou.Trie.path
  | Def of Yuujinchou.Trie.path * Data.value Lazy.t
  | Pi of t * (* binding *) t
  | Lam of (* binding *) t
  | App of t * t
  | Sigma of t * (* binding *) t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Univ of t
  | VirPi of t * (* binding *) t
  | TpULvl
  | ULvl of (Data.ULvlShift.t, t) Mugen.Syntax.endo
  | VirUniv

let var v = Var v
let axiom p = Axiom p
let def p v = Def (p, v)
let pi base fam = Pi (base, fam)
let lam b = Lam b
let app t0 t1 = App (t0, t1)
let sigma base fam = Sigma (base, fam)
let pair t0 t1 = Pair (t0, t1)
let fst t = Fst t
let snd t = Snd t
let univ t = Univ t
let vir_pi base fam = VirPi (base, fam)
let tp_ulvl = TpULvl
let ulvl l = ULvl l
let vir_univ = VirUniv

module ULvl =
  Mugen.Builder.Endo.Make
    (struct
      module Shift = Data.ULvlShift
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)

let dump_name fmt n =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ".") Format.pp_print_string) n

let dump_shift fmt i =
  Format.fprintf fmt "%i" (Mugen.Shift.Int.to_int i)

let rec dump fmt =
  function
  | Var ix ->
    Format.fprintf fmt "Var @[%i@]" ix
  | Lam tm ->
    Format.fprintf fmt "@[<5>Lam (@[%a@])@]" dump tm
  | App (tm1, tm2) ->
    Format.fprintf fmt "@[<5>App (@[%a@],@ @[%a@])@]" dump tm1 dump tm2
  | Pair (tm1, tm2) ->
    Format.fprintf fmt "@[<6>Pair (@[%a@],@ @[%a@])@]" dump tm1 dump tm2
  | Fst tm ->
    Format.fprintf fmt "Fst @[%a@]" dump tm
  | Snd tm ->
    Format.fprintf fmt "Snd @[%a@]" dump tm
  | Univ l ->
    Format.fprintf fmt "Univ @[%a@]" dump l
  | VirPi (base, fam) ->
    Format.fprintf fmt "@[<7>VirPi (@[%a@],@ @[%a@])@]" dump base dump fam
  | Pi (base, fam) ->
    Format.fprintf fmt "@[<7>Pi (@[%a@],@ @[%a@])@]" dump base dump fam
  | Sigma (base, fam) ->
    Format.fprintf fmt "@[<7>Sigma (@[%a@],@ @[%a@])@]" dump base dump fam
  | TpULvl ->
    Format.fprintf fmt "TpULvl"
  | VirUniv ->
    Format.fprintf fmt "VirUniv"
  | Axiom p ->
    Format.fprintf fmt "Axiom %a" dump_name p
  | Def (p, _) ->
    Format.fprintf fmt "Def %a" dump_name p
  | ULvl l ->
    Format.fprintf fmt "ULvl %a" dump_endo l

and dump_endo fmt =
  Mugen.Syntax.Endo.dump
    dump_shift
    dump
    fmt