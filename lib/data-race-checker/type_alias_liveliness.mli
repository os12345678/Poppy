val update_capabilities_if_live_aliases :
  ('a list -> 'a -> bool) -> 'b list -> 'a list -> 'a list
val type_alias_liveness_identifier :
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.A.Var_name.t list ->
  Data_race_env.T.typed_identifier ->
  Data_race_env.T.typed_identifier * Data_race_env.A.Var_name.t list
val type_alias_liveness_expr :
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.A.Var_name.t list ->
  Data_race_env.T.expr ->
  Data_race_env.T.expr_node * Data_race_env.A.Var_name.t list
val type_alias_liveness_block_expr :
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.A.Var_name.t list ->
  Data_race_env.T.block_expr ->
  Data_race_env.T.block_expr * Data_race_env.A.Var_name.t list
val type_alias_liveness_loop_expr :
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Var_name.t list ->
  (Data_race_env.A.capability list -> Data_race_env.A.capability -> bool) ->
  Data_race_env.A.Var_name.t list ->
  Data_race_env.T.block_expr ->
  Data_race_env.T.block_expr * Data_race_env.A.Var_name.t list
