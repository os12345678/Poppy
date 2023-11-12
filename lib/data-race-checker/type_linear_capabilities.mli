module T = Data_race_env.T
module A = Data_race_env.A
module E = Data_race_env
module U = Update_identifier_capabilities
module L = Type_alias_liveliness
val type_linear_obj_method_args :
  E.E.env ->
  Data_race_env.A.Var_name.t ->
  E.A.Struct_name.t ->
  E.T.typed_identifier list -> A.loc -> (unit, Base.Error.t) result
val type_linear_args :
  E.E.env ->
  E.T.typed_identifier list -> A.loc -> (unit, Base.Error.t) result
val type_linear_object_references :
  Data_race_env.A.Var_name.t ->
  E.E.StructNameMap.Key.t ->
  E.E.env -> E.T.block_expr -> Data_race_env.T.block_expr
val type_linear_assign_expr :
  E.E.env -> T.expr_node -> (unit, Base.Error.t) result
val type_linear_capabilities_block_expr :
  E.E.env -> T.block_expr -> (T.block_expr, Base.Error.t) result
