open Core
open Poppy_parser.Ast_types
open Data_race_env

module E = Poppy_type_checker.Type_env

let check_capability_in_struct_capabilities struct_name class_capabilities capability_name =
  match
    List.filter
      ~f:(fun (TCapability (_, name)) -> Capability_name.(=) name capability_name)
      class_capabilities
  with
  | []              ->
      Error
        (Error.of_string
           (Fmt.str "Error: capability %s is not present in %s@."
              (Capability_name.to_string capability_name)
              (Struct_name.to_string struct_name)))
  | capability :: _ -> Ok capability

let type_param_capability_annotations struct_defns = function
  | Param (param_type, _, optional_capability_guards, _) -> (
      let open Result in
      match param_type with
      | TEStruct obj_struct  -> (
          let open Result in
          match optional_capability_guards with
          | Some capability_guards ->
              get_struct_capabilities obj_struct struct_defns
              |> fun struct_capabilities ->
              Result.all
                (List.map
                   ~f:
                     (check_capability_in_struct_capabilities obj_struct struct_capabilities)
                   capability_guards)
              >>| fun _ -> ()
          | None                   -> Ok () )
      | TEInt | TEBool | TEVoid -> Ok () )

let type_params_capability_annotations struct_defns params =
  Result.all_unit (List.map ~f:(type_param_capability_annotations struct_defns) params)