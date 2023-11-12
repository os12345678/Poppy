val filter_capabilities_with_thread_or_subord_state :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.E.env ->
  Data_race_env.A.capability list -> Data_race_env.A.capability -> bool
val remove_thread_subord_caps_from_async_expr :
  Data_race_env.E.env ->
  Data_race_env.T.async_expr -> Data_race_env.T.async_expr
val type_async_capabilities_expr :
  Data_race_env.E.env -> Data_race_env.T.expr -> Data_race_env.T.expr
val type_async_capabilities_block_expr :
  Data_race_env.E.env ->
  Data_race_env.T.block_expr -> Data_race_env.T.block_expr
