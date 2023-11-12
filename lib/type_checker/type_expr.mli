val dedup_free_vars :
  (Free_obj_vars.E.Var_name.t * 'a * 'b) list ->
  (Free_obj_vars.E.Var_name.t * 'a * 'b) list
val string_of_type_list : Free_obj_vars.E.type_expr list -> string
val type_identifier :
  Free_obj_vars.A.identifier ->
  Free_obj_vars.E.Var_name.t list ->
  Type_env.env ->
  Free_obj_vars.E.loc ->
  (Typed_ast.typed_identifier * Free_obj_vars.E.type_expr, Base.Error.t)
  result
val type_args :
  ('a -> 'b -> ('c, 'd) result) -> 'a list -> 'b -> ('c list, 'd) result
val type_constructor_args :
  Free_obj_vars.A.struct_defn ->
  Free_obj_vars.E.Struct_name.t ->
  Free_obj_vars.A.constructor_arg list ->
  (Free_obj_vars.A.expr ->
   Type_env.env -> (Typed_ast.expr, Base.Error.t) result) ->
  Free_obj_vars.E.loc ->
  Type_env.env -> (Typed_ast.constructor_arg list, Base.Error.t) result
val type_expr :
  Free_obj_vars.A.struct_defn list ->
  Free_obj_vars.A.trait_defn list ->
  Free_obj_vars.A.impl_defn list ->
  Free_obj_vars.A.function_defn list ->
  Free_obj_vars.E.Var_name.t list ->
  Free_obj_vars.A.expr ->
  Type_env.env -> (Typed_ast.expr, Base.Error.t) result
val type_async_expr :
  (Free_obj_vars.A.block_expr ->
   Type_env.env ->
   (Typed_ast.block_expr * Free_obj_vars.E.type_expr, Base.Error.t) result) ->
  Type_env.env ->
  Free_obj_vars.A.async_expr -> (Typed_ast.async_expr, Base.Error.t) result
val type_block_expr :
  Free_obj_vars.A.struct_defn list ->
  Free_obj_vars.A.trait_defn list ->
  Free_obj_vars.A.impl_defn list ->
  Free_obj_vars.A.function_defn list ->
  Free_obj_vars.E.Var_name.t list ->
  Free_obj_vars.A.block_expr ->
  Type_env.env ->
  (Typed_ast.block_expr * Free_obj_vars.E.type_expr, Base.Error.t) result
