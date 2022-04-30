type t =
  | Underscore
  | TermField of (Checker.Syntax.t -> Checker.Syntax.t_)
  | TermFun1 of (Checker.Syntax.t -> Checker.Syntax.t_)
  | TermVirtualType of Checker.Syntax.t_
  | CmdDef
  | CmdAxiom
  | CmdQuit
  | CmdImport
  | CmdSectionStart of {tag : string list}
  | CmdSectionEnd of {check_tag : string list -> bool}
