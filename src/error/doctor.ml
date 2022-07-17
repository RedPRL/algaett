module ErrorCode =
struct
  type t =
    | NotInScope
    | NotInferable
    | IllTyped
    | Conversion
  
  let severity = let open Asai.Severity in function
    | NotInScope -> Error
    | NotInferable -> Error
    | IllTyped -> Error
    | Conversion -> Error
  
  let code_num = function
    | NotInScope -> 0
    | NotInferable -> 1
    | IllTyped -> 2
    | Conversion -> 3

  let description = function
    | NotInScope -> 
      "We encountered an out of scope variable"
    | NotInferable ->
      "We encountered a term for which we could not infer a type"
    | IllTyped ->
      "We encountered an ill typed term"
    | Conversion ->
      "We expected two terms to be convertible but they are not"
end

include Asai.Effects.Make(ErrorCode)

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
  | `VirPi -> Format.fprintf fmt "Virtual Pi"
  | `Sigma -> Format.fprintf fmt "Sigma"
  | `Univ -> Format.fprintf fmt "Univ"
  | `VirUniv -> Format.fprintf fmt "Virtual Univ"
  | `TpULvl -> Format.fprintf fmt "TpULvl"

let expected_connective_check conn fmt x =
  let message = Format.asprintf "You are trying to construct an element of a %a type but you are checking against %a" dump_conn conn fmt x in
  let cause = Format.asprintf "This term cannot be checked against %a" fmt x in
  build ~code:IllTyped ~cause ~message |> fatal

let expected_connective_infer conn fmt x =
  let message = Format.asprintf "You are trying to eliminate an element of a %a type but you have an element of %a" dump_conn conn fmt x in
  let cause = Format.asprintf "This term being eliminated in this expression does not have a %a type" dump_conn conn in
  build ~code:IllTyped ~cause ~message |> fatal