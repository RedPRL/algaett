type tag = {tag : string list}
type check_tag = {check_tag : string list -> bool}

(* names and keywords *)
val name : string list Earley_core.Earley.grammar
val underscore : unit Earley_core.Earley.grammar
val term_field : (Elaborator.Syntax.t -> Elaborator.Syntax.t_) Earley_core.Earley.grammar
val term_constant : (shift:Elaborator.Syntax.shift list option -> Elaborator.Syntax.t_) Earley_core.Earley.grammar
val term_vir_tp : Elaborator.Syntax.t_ Earley_core.Earley.grammar
val def : unit Earley_core.Earley.grammar
val axiom : unit Earley_core.Earley.grammar
val quit : unit Earley_core.Earley.grammar
val import : unit Earley_core.Earley.grammar
val section_start : tag Earley_core.Earley.grammar
val section_end : check_tag Earley_core.Earley.grammar

(* symbols *)
val assign : unit Earley_core.Earley.grammar
val at : unit Earley_core.Earley.grammar
val colon : unit Earley_core.Earley.grammar
val comma : unit Earley_core.Earley.grammar
val dollar : unit Earley_core.Earley.grammar
val equal : unit Earley_core.Earley.grammar
val maps_to : unit Earley_core.Earley.grammar
val multiply : unit Earley_core.Earley.grammar
val plus : unit Earley_core.Earley.grammar
val right_arrow : unit Earley_core.Earley.grammar
val question : unit Earley_core.Earley.grammar
val semi : unit Earley_core.Earley.grammar
val semisemi : unit Earley_core.Earley.grammar
val times : unit Earley_core.Earley.grammar
val up : unit Earley_core.Earley.grammar

(* numbers *)
val pos_num : int Earley_core.Earley.grammar
val num : int Earley_core.Earley.grammar
