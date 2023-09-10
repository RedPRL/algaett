module Code =
struct
  type t =
    | NotInScope
    | NotInferable
    | IllTyped
    | Conversion

  let default_severity =
    let open Asai.Severity in
    function
    | NotInScope -> Error
    | NotInferable -> Error
    | IllTyped -> Error
    | Conversion -> Error

  let to_string =
    function
    | NotInScope ->
      "We encountered an out of scope variable"
    | NotInferable ->
      "We encountered a term for which we could not infer a type"
    | IllTyped ->
      "We encountered an ill-typed term"
    | Conversion ->
      "We expected two terms to be convertible but they are not"
end

include Asai.Logger.Make(Code)

type connective =
  [ `Pi
  | `VirPi
  | `Sigma
  | `Univ
  | `VirUniv
  | `TpULvl
  ]

let dump_conn fmt : connective -> _ = function
  | `Pi -> Format.fprintf fmt "Pi"
  | `VirPi -> Format.fprintf fmt "(virtual) Pi"
  | `Sigma -> Format.fprintf fmt "Sigma"
  | `Univ -> Format.fprintf fmt "universe"
  | `VirUniv -> Format.fprintf fmt "(virtual) universe"
  | `TpULvl -> Format.fprintf fmt "universe level"

let expected_connective_check conn fmt x =
  fatalf IllTyped "The expression seems to have a %a type but is checked against %a" dump_conn conn fmt x

let expected_connective_infer conn fmt x =
  fatalf IllTyped "The expression is of type %a but is used as an element of a %a type" fmt x dump_conn conn
