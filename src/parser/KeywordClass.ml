type t =
  | Underscore
  | TermField of (Elaborator.Syntax.t -> Elaborator.Syntax.t_)
  | TermConstant of (shift:Elaborator.Syntax.shift list option -> Elaborator.Syntax.t_)
  | TermVirtualType of Elaborator.Syntax.t_
  | CmdDef
  | CmdAxiom
  | CmdQuit
  | CmdImport
  | CmdSectionStart of {tag : string list}
  | CmdSectionEnd of {check_tag : string list -> bool}
