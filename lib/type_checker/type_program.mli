val add_definitions_to_global :
  init:'a -> f:('a -> 'b -> 'a) -> 'b list -> 'a
val type_check_definitions :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  (Typed_ast.trait_defn list * Typed_ast.function_defn list *
   Typed_ast.impl_defn list, Base.Error.t)
  result
val type_check_main_expr :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  Poppy_parser.Ast_types.Var_name.t list ->
  Poppy_parser.Ast.block_expr ->
  (Typed_ast.block_expr * Poppy_parser.Ast_types.type_expr, Base.Error.t)
  result
val type_program :
  Poppy_parser.Ast.program ->
  (Type_env.env * Typed_ast.program, Base.Error.t) result
