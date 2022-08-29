type code =
  | NotInScope
  | NotInferable
  | IllTyped
  | Conversion

module Code : Asai.Code.S with type t = code =
struct
  type t = code
  let default_severity = let open Asai.Severity in function
    | NotInScope -> Error
    | NotInferable -> Error
    | IllTyped -> Error
    | Conversion -> Error

  let to_string = function
    | NotInScope -> 
      "We encountered an out of scope variable"
    | NotInferable ->
      "We encountered a term for which we could not infer a type"
    | IllTyped ->
      "We encountered an ill typed term"
    | Conversion ->
      "We expected two terms to be convertible but they are not"
end

include Asai.Logger.Make(Code)