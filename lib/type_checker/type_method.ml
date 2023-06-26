open Poppy_parser
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

let rec add_params_to_scope env params =
  match params with
  | [] -> Ok env
  | Ast_types.Param (param_type, var_name, _, _) :: rest ->
    let updated_env = add_var_to_block_scope env var_name param_type in
    add_params_to_scope updated_env rest

let check_method_signature_matches param_list return_type method_signature =
  match method_signature with
  | Ast.TMethodSignature (_, _, _, trait_param_list, trait_return_type) ->
    if not (phys_equal param_list trait_param_list) then
      Error (Core.Error.of_string "Parameter list does not match trait method signature")
    else if not (phys_equal return_type trait_return_type) then
      Error (Core.Error.of_string "Return type does not match trait method signature")
    else
      Ok ()

let type_method_defn env struct_defns trait_defns method_defns function_defns (Ast.TImpl (trait_name, struct_name, methods)) =
  let%bind struct_defn = lookup_struct env struct_name in
  let%bind implemented_traits = lookup_impl env struct_name in
  if not (List.mem implemented_traits trait_name ~equal:Ast_types.Trait_name.(=)) then
    Error (Core.Error.of_string "Trait not implemented for struct")
  else
  begin match struct_defn with
  | Ast.TStruct (struct_defn_name, _, _) ->
    if not (Ast_types.Struct_name.(=) struct_defn_name struct_name) then
      Error (Core.Error.of_string (Fmt.str "Methods are not defined for struct %s" (Ast_types.Struct_name.to_string struct_name)))
    else
      let%bind trait_defn = lookup_trait env trait_name in
      Result.all (List.map methods ~f:(fun (Ast.TMethod (TMethodSignature (method_name, borrowed, capabilities, param_list, return_type), body)) ->
        let%bind method_signature = lookup_method_signature trait_defn method_name in
        let%bind () = check_method_signature_matches param_list return_type method_signature in
        let%bind updated_env = add_params_to_scope env param_list in
        let%map typed_body = Type_expr.type_block_expr struct_defns trait_defns method_defns function_defns body updated_env in
        Ok (Typed_ast.TMethod (trait_name, struct_name, TMethodSignature (method_name, borrowed, capabilities, param_list, return_type), typed_body))
      ))
  end
      
  