module E = Data_race_env.E
val check_capability_in_struct_capabilities :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.capability list ->
  Data_race_env.A.Capability_name.t ->
  (Data_race_env.A.capability, Base.Error.t) result
val type_param_capability_annotations :
  Data_race_env.T.struct_defn list ->
  Data_race_env.A.param -> (unit, Base.Error.t) result
val type_params_capability_annotations :
  Data_race_env.T.struct_defn list ->
  Data_race_env.A.param list -> (unit, Base.Error.t) result
