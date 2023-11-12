val flatten_method_defns :
  Data_race_env.T.impl_defn list -> Data_race_env.T.method_defn list
val type_data_race_program :
  Data_race_env.E.env ->
  Data_race_env.T.program ->
  ignore_data_races:bool -> (Data_race_env.T.program, Base.Error.t) result
