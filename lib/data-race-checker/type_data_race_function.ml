open Core
open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Type_capability_annotations
open Type_capability_constraints
open Type_borrowing
open Data_race_env
open Type_data_race_expr

let type_data_races_function_defn struct_defns _trait_defns impl_defns method_defns function_defns (TFunction (func_sig, body_expr)) (env: E.env) ~ignore_data_races=
  let open Result in
  type_params_capability_annotations struct_defns func_sig.params
  >>= fun () ->
  let error_prefix =
    Fmt.str "Potential data race in function %s " (Function_name.to_string func_sig.name)
  in
  let param_obj_var_capabilities =
    params_to_obj_vars_and_capabilities struct_defns func_sig.params in
  type_function_reverse_borrowing struct_defns error_prefix func_sig.return_type func_sig.borrowed
    body_expr
  >>= fun () ->
  type_param_capability_constraints param_obj_var_capabilities body_expr
  |> fun param_constrained_body_expr ->
  type_data_races_block_expr struct_defns _trait_defns impl_defns method_defns function_defns env param_constrained_body_expr
    param_obj_var_capabilities ~ignore_data_races
  >>| fun data_race_checked_body_expr ->
  TFunction
    (func_sig, data_race_checked_body_expr)