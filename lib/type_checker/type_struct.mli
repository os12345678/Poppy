val check_no_duplicate_struct_names :
  Poppy_parser.Ast.struct_defn list -> unit Core.Or_error.t
val check_no_duplicate_fields :
  Poppy_parser.Ast.struct_defn list -> unit Core.Or_error.t
val check_field_types_are_valid :
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast_types.field_defn list -> (unit, Base.Error.t) result
val type_struct_defn :
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.struct_defn ->
  (Typed_ast.struct_defn, Base.Error.t) result
val type_struct_defns :
  Poppy_parser.Ast.struct_defn list ->
  (Typed_ast.struct_defn list, Base.Error.t) result
