val remove_subord_capabilities :
  Data_race_env.E.env ->
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.capability list -> Data_race_env.A.capability list
val remove_subord_capabilities_id :
  Data_race_env.E.env ->
  Data_race_env.T.typed_identifier -> Data_race_env.T.typed_identifier
val remove_subord_capabilities_expr :
  Data_race_env.E.env -> Data_race_env.T.expr -> Data_race_env.T.expr
val remove_subord_capabilities_block_expr :
  Data_race_env.E.env ->
  Data_race_env.T.block_expr -> Data_race_env.T.block_expr
val is_this_present : (Data_race_env.A.Var_name.t * 'a * 'b) list -> bool
val type_subord_capabilities_block_expr :
  Data_race_env.E.env ->
  (Data_race_env.A.Var_name.t * 'a * 'b) list ->
  Data_race_env.T.block_expr -> Data_race_env.T.block_expr
val type_subord_capabilities_method_prototype :
  Data_race_env.E.env ->
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.Method_name.t ->
  Data_race_env.A.type_expr ->
  (Data_race_env.A.Var_name.t * Data_race_env.A.Struct_name.t *
   Data_race_env.A.capability list)
  list -> (unit, Base.Error.t) result
