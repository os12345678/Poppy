val access_to_string : Poppy_parser.Ast.access_modifier -> string
val typ_to_string : Poppy_parser.Ast.typ -> string
val class_info_to_string : Scoping.class_info -> string
val find_member_access_and_type :
  Scoping.class_info ->
  string -> (Poppy_parser.Ast.access_modifier * Poppy_parser.Ast.typ) option
val is_same_or_subclass_of :
  Scoping.class_info option -> Scoping.class_info option -> bool
val is_member_accessible :
  Scoping.class_info -> Scoping.class_info -> string -> bool
val initialize_global_scope : unit -> Scoping.scope
val check_type : Scoping.scope -> Poppy_parser.Ast.typ -> unit
val type_check_expr :
  Scoping.scope ->
  Poppy_parser.Ast.expr -> Scoping.class_info -> Poppy_parser.Ast.typ
val type_check_statement :
  Scoping.scope -> Poppy_parser.Ast.statement -> Scoping.class_info -> unit
val type_check_program : Poppy_parser.Ast.statement list -> unit
