val update_capabilities_if_match :
  'a list -> ('b list -> 'b -> bool) -> 'a -> 'b list -> 'b list
val update_matching_identifier_caps :
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.T.typed_identifier -> Data_race_env.T.typed_identifier
val update_matching_identifier_caps_expr :
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.T.expr -> Data_race_env.T.expr
val update_matching_identifier_caps_block_expr :
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.T.block_expr -> Data_race_env.T.block_expr
