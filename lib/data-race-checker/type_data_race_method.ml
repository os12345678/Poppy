open Core
open Type_data_race_expr
open Type_capability_annotations
open Type_subord_capabilities
open Type_borrowing
open Type_capability_constraints
open Poppy_parser.Ast_types
open Data_race_env
open Poppy_type_checker.Typed_ast

(* E.env ->
Struct_name.t ->
Method_name.t ->
type_expr ->
(Var_name.t * Struct_name.t * capability list) list -> *)

let type_data_races_method_defn struct_name trait_name (env: E.env) 
    (struct_defns: struct_defn list) trait_defns impl_defns method_defns function_defns ~ignore_data_races
    (TMethod (method_sig, body_expr)) =
  let open Result in
  let param_obj_var_capabilities =
    params_to_obj_vars_and_capabilities struct_defns method_sig.params in
  type_params_capability_annotations struct_defns method_sig.params
  >>= fun () ->
  let error_prefix =
    Fmt.str "Potential data race in %s's method %s "
      (Struct_name.to_string struct_name)
      (Method_name.to_string method_sig.name) in
  type_function_reverse_borrowing struct_defns error_prefix method_sig.return_type method_sig.borrowed
    body_expr
  >>= fun () ->
  type_subord_capabilities_method_prototype struct_defns struct_name method_sig.name method_sig.return_type
    param_obj_var_capabilities
  >>= fun () ->
  type_param_capability_constraints
    ( (Var_name.of_string "this", struct_name, (get_method_capabilities2 method_sig.name struct_name trait_name impl_defns struct_defns))
    :: param_obj_var_capabilities )
    body_expr
  |> fun param_constrained_body_expr ->
  type_data_races_block_expr struct_defns trait_defns impl_defns method_defns function_defns env param_constrained_body_expr
    ~ignore_data_races
    ( (Var_name.of_string "this", struct_name, (get_method_capabilities2 method_sig.name struct_name trait_name impl_defns struct_defns))
    :: param_obj_var_capabilities )
  >>| fun data_race_checked_body_expr ->
  TMethod
    ( method_sig
    , data_race_checked_body_expr )