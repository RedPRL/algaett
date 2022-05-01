type t =
  | Underscore
  | TermField of (Checker.Syntax.t -> Checker.Syntax.t_)
  | TermConstant of (shift:int list option -> Checker.Syntax.t_)
  | TermVirtualType of Checker.Syntax.t_
  | CmdDef
  | CmdAxiom
  | CmdQuit
  | CmdImport
  | CmdSectionStart of {tag : string list}
  | CmdSectionEnd of {check_tag : string list -> bool}
