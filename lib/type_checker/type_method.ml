open Poppy_parser
open Poppy_parser.Ast_types
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

let check_method_signature_matches param_list return_type method_signature =
  let trait_param_types = List.map method_signature.params ~f:(fun (Param (param_type, _, _, _)) -> param_type) in
  if equal_type_expr_list trait_param_types (List.map param_list ~f:(fun (Param (param_type, _, _, _)) -> param_type)) &&
      equal_type_expr return_type method_signature.return_type then
    Ok ()
  else
    Error (Core.Error.of_string "Method signature does not match")
    
let type_method_defn env struct_defns trait_defns method_defns function_defns (Ast.TImpl (trait_name, struct_name, methods)) =
  let%bind struct_defn = lookup_struct env struct_name in
  let%bind implemented_traits = lookup_impl env struct_name in
  if not (List.mem implemented_traits trait_name ~equal:Trait_name.(=)) then
    Error (Core.Error.of_string "Trait not implemented for struct")
  else
    begin match struct_defn with
    | Ast.TStruct (struct_defn_name, _, _) ->
      if not (Struct_name.(=) struct_defn_name struct_name) then
        Error (Core.Error.of_string (Fmt.str "Methods are not defined for struct %s" (Struct_name.to_string struct_name)))
      else
        let%bind trait_defn = lookup_trait env trait_name in
        let typed_methods_result = Result.all (List.map methods ~f:(fun (Ast.TMethod (method_signature, body)) ->
        let env = add_block_scope env VarNameMap.empty in
        let%bind method_signature = lookup_method_signature trait_defn method_signature.name in
        let%bind () = check_method_signature_matches method_signature.params method_signature.return_type method_signature in
        let env_with_this = add_this_to_block_scope env struct_name in
        let%bind updated_env = add_params_to_scope env_with_this method_signature.params in
        let%map (typed_body, _) = Type_expr.type_block_expr struct_defns trait_defns method_defns function_defns body updated_env in
        Typed_ast.TMethod(method_signature, typed_body)
      )) in
      match typed_methods_result with
      | Ok typed_methods -> Ok (Typed_ast.TImpl (trait_name, struct_name, typed_methods))
      | Error e -> Error e
    end
    
let type_method_defns env struct_defns trait_defns method_defns function_defns = 
  Result.all (List.map ~f: (type_method_defn env struct_defns trait_defns method_defns function_defns) method_defns)
      
      
      