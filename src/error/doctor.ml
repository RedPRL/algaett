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