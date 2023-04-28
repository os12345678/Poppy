val access_to_string : Poppy_parser.Ast.access_modifier -> string
val typ_to_string : Poppy_parser.Ast.typ -> string
val initialize_global_scope : unit -> Scoping.scope
val check_type : Scoping.scope -> Poppy_parser.Ast.typ -> unit
val type_check_expr :
  Scoping.scope -> Poppy_parser.Ast.expr -> Poppy_parser.Ast.typ
val type_check_statement :
  Scoping.scope -> Poppy_parser.Ast.statement -> unit
val type_check_program : Poppy_parser.Ast.statement list -> unit
