type t =
  [ `Pi
  | `VirPi
  | `Sigma
  | `Univ
  | `VirUniv
  | `TpULvl
  ]

let dump_conn fmt : t -> _ = function
  | `Pi -> Format.fprintf fmt "Pi"
  | `VirPi -> Format.fprintf fmt "Virtual Pi"
  | `Sigma -> Format.fprintf fmt "Sigma"
  | `Univ -> Format.fprintf fmt "Univ"
  | `VirUniv -> Format.fprintf fmt "Virtual Univ"
  | `TpULvl -> Format.fprintf fmt "TpULvl"

let check ?loc conn fmt x =
  Logger.fatalf ?loc ~code:IllTyped "Trying to construct an element of a %a type but you are checking against %a" dump_conn conn fmt x

let infer ?loc conn fmt x =
  Logger.fatalf ?loc ~code:IllTyped "Trying to eliminate an element of a %a type but you have an element of %a" dump_conn conn fmt x