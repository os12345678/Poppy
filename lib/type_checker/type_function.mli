val type_function_defn :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  Poppy_parser.Ast.function_defn ->
  (Typed_ast.function_defn, Core.Error.t) result
val type_function_defns :
  Type_env.env ->
  Poppy_parser.Ast.struct_defn list ->
  Poppy_parser.Ast.trait_defn list ->
  Poppy_parser.Ast.impl_defn list ->
  Poppy_parser.Ast.function_defn list ->
  (Typed_ast.function_defn list, Core.Error.t) result
