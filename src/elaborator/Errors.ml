type t =
  | NotInferable of {tm : Syntax.t}
  | IllTyped of {tm : Syntax.t; tp : NbE.Domain.t}