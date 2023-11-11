val check_arg_borrowing :
  Data_race_env.E.env ->
  Data_race_env.A.loc ->
  Data_race_env.A.param * Data_race_env.T.expr -> (unit, Base.Error.t) result
val type_function_forward_borrowing_expr :
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.E.env -> Data_race_env.T.expr -> (unit, Base.Error.t) result
val type_function_forward_borrowing_block_expr :
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.E.env ->
  Data_race_env.T.block_expr -> (unit, Base.Error.t) result
val id_maybe_borrowed :
  Data_race_env.T.typed_identifier -> Data_race_env.A.borrowed_ref option
val id_is_borrowed : Data_race_env.T.typed_identifier -> bool
val type_function_reverse_borrowing :
  Data_race_env.E.env ->
  string ->
  Data_race_env.A.type_expr ->
  Data_race_env.A.borrowed_ref option ->
  Data_race_env.T.block_expr -> (unit, Base.Error.t) result
val type_assign_borrowed_expr :
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  'a ->
  Data_race_env.T.expr ->
  (Data_race_env.A.borrowed_ref option, Base.Error.t) result
val type_assign_borrowed_block_expr :
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  'a ->
  Data_race_env.T.block_expr ->
  (Data_race_env.A.borrowed_ref option, Base.Error.t) result
