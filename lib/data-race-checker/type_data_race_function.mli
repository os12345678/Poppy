val type_data_races_function_defn :
  Data_race_env.T.struct_defn list ->
  'a ->
  Data_race_env.T.impl_defn list ->
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  Data_race_env.T.function_defn ->
  Data_race_env.E.env ->
  ignore_data_races:bool ->
  (Data_race_env.T.function_defn, Base.Error.t) result
