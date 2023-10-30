module A = Poppy_parser.Ast
module T = Typed_ast
module E = Poppy_parser.Ast_types
val remove_bound_var :
  Poppy_parser.Ast_types.Var_name.t ->
  (Poppy_parser.Ast_types.Var_name.t * 'a * 'b) list ->
  (Poppy_parser.Ast_types.Var_name.t * 'a * 'b) list
val union_free_vars_lists : 'a list list -> 'a list
val free_obj_vars_identifier :
  T.typed_identifier ->
  Type_env.env ->
  (Poppy_parser.Ast_types.Var_name.t * Poppy_parser.Ast_types.Struct_name.t *
   Poppy_parser.Ast_types.capability list)
  list
val free_obj_vars_expr :
  Type_env.env ->
  T.expr ->
  (Poppy_parser.Ast_types.Var_name.t * Poppy_parser.Ast_types.Struct_name.t *
   Poppy_parser.Ast_types.capability list)
  list
val free_obj_vars_block_expr :
  Type_env.env ->
  T.block_expr ->
  (Poppy_parser.Ast_types.Var_name.t * Poppy_parser.Ast_types.Struct_name.t *
   Poppy_parser.Ast_types.capability list)
  list
