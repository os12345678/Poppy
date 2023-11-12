val check_method_signature_matches :
  Poppy_parser.Ast_types.param list ->
  Poppy_parser.Ast_types.type_expr ->
  Poppy_parser.Ast_types.method_signature -> (unit, Base.Error.t) result
val type_method_defn :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  Poppy_parser.Ast.impl_defn -> (Typed_ast.impl_defn, Base.Error.t) result
val type_method_defns :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  (Typed_ast.impl_defn list, Base.Error.t) result
