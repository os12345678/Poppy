open Core

open Poppy_parser.Ast_types
(* open Poppy_type_checker.Typed_ast *)
open Data_race_env

(* There is another capability in the class that can both access subordinate state in one
   capability and also subordinate state in another capability - thus acting as a channel
   for them. *)
   let capabilities_have_subord_channel class_name class_defns capability_1_name
   capability_2_name =
 let get_cap_subord_fields capability_name =
   List.filter
     ~f:(fun (TField (_, field_type, _, _)) ->
       type_has_mode field_type Subordinate class_defns)
     (get_class_capability_fields class_name capability_name class_defns) in
 (* collect the capabilities that aren't capability 1 or capability 2 and have access to
    subord state*)
 let get_potential_channel_capabilities sub_ord_fields =
   List.concat_map
     ~f:(fun (TField (_, _, _, field_cap_names)) ->
       List.filter
         ~f:(fun cap_name ->
           (not (Capability_name.(=) cap_name capability_1_name)) && not (Capability_name.(=) cap_name capability_2_name))
         field_cap_names)
     sub_ord_fields in
 let get_capability_1_potential_channels =
   get_potential_channel_capabilities (get_cap_subord_fields capability_1_name) in
 let get_capability_2_potential_channels =
   get_potential_channel_capabilities (get_cap_subord_fields capability_1_name) in
 (* check if a capability in intersection of these potential channels *)
 let subord_channels =
   intersect_lists get_capability_1_potential_channels
     get_capability_2_potential_channels in
 List.length subord_channels > 0


(* Check that overlapping fields are safe - i.e. either we're accessing them with a safe
   mode, or they themselves are safe *)
   let capabilities_have_safe_shared_state struct_name env
   (TCapability (capability_1_mode, capability_1_name))
   (TCapability (capability_2_mode, capability_2_name)) =
 let capabilities_modes_are_safe capability_1_mode capability_2_mode =
   capability_mode_present capability_1_mode ThreadSafe
   && capability_mode_present capability_2_mode ThreadSafe in
 let capability_1_fields =
   get_class_capability_fields struct_name capability_1_name env in
 let capability_2_fields =
   get_class_capability_fields struct_name capability_2_name env in
 let shared_fields = intersect_lists capability_1_fields capability_2_fields in
 capabilities_modes_are_safe capability_1_mode capability_2_mode
 || List.for_all
      ~f:(fun (TField (modifier, field_type, _, _)) ->
        match field_type with
        | TEBool | TEInt | TEVoid ->
            phys_equal modifier MConst (* if immutable primitive then safe *)
        | TEStruct _               -> type_has_mode field_type ThreadSafe env)
      shared_fields

(* Check that overlapping fields are not subordinate *)
let capabilities_have_no_subord_shared_state struct_name env capability_1_name
    capability_2_name =
  let capability_1_fields =
    get_class_capability_fields struct_name capability_1_name env in
  let capability_2_fields =
    get_class_capability_fields struct_name capability_2_name env in
  let shared_fields = intersect_lists capability_1_fields capability_2_fields in
  List.for_all
    ~f:(fun (TField (_, field_type, _, _)) ->
      not (type_has_mode field_type Subordinate env))
    shared_fields

let can_concurrently_access_capabilities class_name class_defns
    (TCapability (capability_1_mode, capability_1_name) as capability1)
    (TCapability (_, capability_2_name) as capability2) =
  capabilities_have_safe_shared_state class_name class_defns capability1 capability2
  && capabilities_have_no_subord_shared_state class_name class_defns capability_1_name
       capability_2_name
  && (not
        (capabilities_have_subord_channel class_name class_defns capability_1_name
           capability_2_name))
  (* Can't access the same linear capability in multiple threads as violates linearity *)
  && not (phys_equal capability_1_mode Linear && phys_equal capability_1_name capability_2_name)
