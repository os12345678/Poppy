open Core 
open Data_race_env
open Update_identifier_capabilities


let type_param_capability_constraints param_names_and_capabilities block_expr =
  List.fold ~init:block_expr
    ~f:(fun acc_expr (param_name, _, param_allowed_caps) ->
      (* for each param, get it and all aliases and filter their capabilities to only the
         allowed capabilities *)
      let param_aliases =
        find_aliases_in_block_expr ~should_match_fields:false param_name block_expr in
      update_matching_identifier_caps_block_expr (param_name :: param_aliases)
        (fun _ cap -> elem_in_list cap param_allowed_caps)
        acc_expr)
    param_names_and_capabilities