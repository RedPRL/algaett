type tag = {tag : string list}
type check_tag = {check_tag : string list -> bool}

(* names and keywords *)
val name : string list Earley_core.Earley.grammar
val underscore : unit Earley_core.Earley.grammar
val term_field : (Checker.Syntax.t -> Checker.Syntax.t_) Earley_core.Earley.grammar
val term_fun1 : (Checker.Syntax.t -> Checker.Syntax.t_) Earley_core.Earley.grammar
val term_vir_tp : Checker.Syntax.t_ Earley_core.Earley.grammar
val def : unit Earley_core.Earley.grammar
val assign : unit Earley_core.Earley.grammar
val axiom : unit Earley_core.Earley.grammar
val quit : unit Earley_core.Earley.grammar
val import : unit Earley_core.Earley.grammar
val section_start : tag Earley_core.Earley.grammar
val section_end : check_tag Earley_core.Earley.grammar

(* symbols *)
val at : unit Earley_core.Earley.grammar
val colon : unit Earley_core.Earley.grammar
val comma : unit Earley_core.Earley.grammar
val right_arrow : unit Earley_core.Earley.grammar
val maps_to : unit Earley_core.Earley.grammar
val times : unit Earley_core.Earley.grammar
