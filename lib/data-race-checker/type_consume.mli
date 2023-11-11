val string_of_id : Data_race_env.T.typed_identifier -> string
val check_identifiers_disjoint :
  Data_race_env.T.typed_identifier ->
  Data_race_env.T.typed_identifier -> bool
val remove_reassigned_id :
  Data_race_env.T.typed_identifier ->
  Data_race_env.T.typed_identifier list ->
  Data_race_env.T.typed_identifier list
val check_identifier_accessible :
  Data_race_env.T.typed_identifier ->
  Data_race_env.T.typed_identifier list -> (unit, Base.Error.t) result
val is_identifier_linear :
  Data_race_env.T.typed_identifier -> Data_race_env.E.env -> bool
val check_identifier_consumable :
  Data_race_env.E.env ->
  Data_race_env.T.typed_identifier ->
  Data_race_env.T.typed_identifier list -> (unit, Base.Error.t) result
val check_shared_var_not_consumed :
  Data_race_env.A.Var_name.t ->
  Data_race_env.T.typed_identifier list -> (unit, Base.Error.t) result
val accumulate_consumed_ids :
  Data_race_env.E.env ->
  (Data_race_env.T.typed_identifier list, Base.Error.t) result ->
  Data_race_env.T.expr ->
  (Data_race_env.T.typed_identifier list, Base.Error.t) result
val type_consume_expr :
  Data_race_env.E.env ->
  Data_race_env.T.expr ->
  Data_race_env.T.typed_identifier list ->
  (Data_race_env.T.typed_identifier list, Base.Error.t) result
val type_consume_block_expr :
  Data_race_env.E.env ->
  Data_race_env.T.block_expr ->
  Data_race_env.T.typed_identifier list ->
  (Data_race_env.T.typed_identifier list, Base.Error.t) result
val type_consume_async_expr :
  Data_race_env.E.env ->
  Data_race_env.T.async_expr ->
  Data_race_env.T.async_expr list ->
  Data_race_env.T.typed_identifier list -> (unit, Base.Error.t) result
