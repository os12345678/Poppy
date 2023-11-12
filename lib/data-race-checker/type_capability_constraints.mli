val type_param_capability_constraints :
  (Data_race_env.A.Var_name.t * 'a * Data_race_env.A.capability list) list ->
  Data_race_env.T.block_expr -> Data_race_env.T.block_expr
val type_obj_method_capability_constraints :
  Data_race_env.T.method_defn list ->
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Method_name.t ->
  Data_race_env.A.Capability_name.t list ->
  Data_race_env.A.loc -> (unit, Base.Error.t) result
val type_capability_constraints_assigned_expr :
  Data_race_env.T.struct_defn list ->
  Data_race_env.A.type_expr ->
  Data_race_env.T.expr -> Data_race_env.A.loc -> (unit, Base.Error.t) result
val type_capability_constraints_function_arg :
  Data_race_env.E.env ->
  string ->
  Data_race_env.A.loc ->
  Data_race_env.A.param * Data_race_env.T.expr -> (unit, Base.Error.t) result
val type_capabilities_constraints_identifier :
  Data_race_env.T.typed_identifier ->
  Data_race_env.A.loc -> (unit, Base.Error.t) result
val type_capabilities_constraints_expr :
  Data_race_env.T.struct_defn list ->
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.E.env -> Data_race_env.T.expr -> (unit, Base.Error.t) result
val type_capabilities_constraints_block_expr :
  Data_race_env.T.struct_defn list ->
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.E.env ->
  Data_race_env.T.block_expr -> (unit, Base.Error.t) result
