val capabilities_have_subord_channel :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.E.env ->
  Data_race_env.A.Capability_name.t ->
  Data_race_env.A.Capability_name.t -> bool
val capabilities_have_safe_shared_state :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.E.env ->
  Data_race_env.A.capability -> Data_race_env.A.capability -> bool
val capabilities_have_no_subord_shared_state :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.E.env ->
  Data_race_env.A.Capability_name.t ->
  Data_race_env.A.Capability_name.t -> bool
val can_concurrently_access_capabilities :
  Data_race_env.A.Struct_name.t ->
  Data_race_env.E.env ->
  Data_race_env.A.capability -> Data_race_env.A.capability -> bool
val type_concurrent_capability_pair_constraints_var :
  Data_race_env.E.env ->
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.capability list ->
  Data_race_env.A.capability list ->
  Data_race_env.A.loc -> (unit, Base.Error.t) result
val type_concurrent_capabilities_constraints_var :
  Data_race_env.E.env ->
  Data_race_env.A.Var_name.t ->
  Data_race_env.A.Struct_name.t ->
  Data_race_env.A.capability list list ->
  Data_race_env.A.loc -> (unit, Base.Error.t) result
val type_concurrent_capability_constraints_vars :
  Data_race_env.E.env ->
  (Data_race_env.A.Var_name.t * Data_race_env.A.Struct_name.t *
   Data_race_env.A.capability list)
  list -> Data_race_env.A.loc -> (unit, Base.Error.t) result
