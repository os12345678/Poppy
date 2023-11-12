val type_data_races_method_defn :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.Trait_name.t ->
  Data_race_env.E.env ->
  Data_race_env.T.struct_defn list ->
  'a ->
  Data_race_env.T.impl_defn list ->
  Data_race_env.T.method_defn list ->
  Data_race_env.T.function_defn list ->
  ignore_data_races:bool ->
  Data_race_env.T.method_defn ->
  (Data_race_env.T.method_defn, Base.Error.t) result
