type tag = {tag : string list}
type check_tag = {check_tag : string list -> bool}

(* names and keywords *)
val name : string list Earley_core.Earley.grammar
val underscore : unit Earley_core.Earley.grammar
val term_field : (Checker.Syntax.t -> Checker.Syntax.t_) Earley_core.Earley.grammar
val term_constant : (shift:int list option -> Checker.Syntax.t_) Earley_core.Earley.grammar
val term_vir_tp : Checker.Syntax.t_ Earley_core.Earley.grammar
val def : unit Earley_core.Earley.grammar
val assign : unit Earley_core.Earley.grammar
val axiom : unit Earley_core.Earley.grammar
val quit : unit Earley_core.Earley.grammar
val import : unit Earley_core.Earley.grammar
val section_start : tag Earley_core.Earley.grammar
val section_end : check_tag Earley_core.Earley.grammar

(* symbols *)
val semisemi : unit Earley_core.Earley.grammar
val up : unit Earley_core.Earley.grammar
val at : unit Earley_core.Earley.grammar
val colon : unit Earley_core.Earley.grammar
val comma : unit Earley_core.Earley.grammar
val right_arrow : unit Earley_core.Earley.grammar
val maps_to : unit Earley_core.Earley.grammar
val times : unit Earley_core.Earley.grammar

(* numbers *)
val num : int Earley_core.Earley.grammar
