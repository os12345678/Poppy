val type_data_races_block_expr :
  Data_race_env.T.struct_defn list ->
  'a ->
  Data_race_env.T.impl_defn list ->
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.E.env ->
  Data_race_env.T.block_expr ->
  (Poppy_parser.Ast_types.Var_name.t * 'b * 'c) list ->
  ignore_data_races:bool -> Data_race_env.T.block_expr Core.Or_error.t
