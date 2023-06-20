(* open Poppy_parser
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

(* 
1. No duplicate trait names
2. No duplicate method names within a trait
3. Valid method signatures
4. No circular dependencies   
*)

let check_no_duplicate_trait_names trait_defns =
  let trait_names = List.map ~f:(fun (Ast.TTrait (trait_name, _)) -> trait_name) trait_defns in
  if has_duplicates trait_names ~equal:Ast_types.Trait_name.(=)  then
    Or_error.errorf "%s Duplicate trait names found" (Ast_types.Trait_name.to_string (List.hd_exn trait_names))
  else
    Ok ()

let check_no_duplicate_method_signatures trait_defns =
  let method_signature = List.map ~f:(fun (Ast.TTrait (_, method_signatures)) -> method_signatures) trait_defns in
  let method_signature_names = List.concat_map ~f:(List.map ~f:(function Ast.TMethodSignature (method_name, _, _, _, _) -> method_name)) method_signature in
  if has_duplicates method_signature_names ~equal:Ast_types.Method_name.(=)  then
    Or_error.errorf "%s Duplicate method names found" (Ast_types.Method_name.to_string (List.hd_exn method_signature_names))
  else
    Ok ()

let type_method_type_sig trait_defns method_name params return_type = 
  let method_error_prefix =
    Fmt.str "Type error for method %s" (Ast_types.Method_name.to_string method_name) in
  let return_type_error_prefix = Fmt.str "%s return type" method_error_prefix in
  let%bind () = check_type_valid trait_defns return_type return_type_error_prefix in
  Result.all_unit  
  (List.map
  ~f:(fun (Ast_types.Param (param_type, param_name, _, _)) ->
    let param_error_prefix = 
      Fmt.str "%s param %s" return_type_error_prefix (Ast_types.Var_name.to_string param_name)
    in
    check_type_valid trait_defns param_type param_error_prefix)
  params)

let type_method_signature (Ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)) =
  Typed_ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)

let type_trait_defn (Ast.TTrait (trait_name, method_sigs) as current_trait_defn) = 
  let%bind () = check_no_duplicate_trait_names [current_trait_defn] in
  let%bind () = check_no_duplicate_method_signatures [current_trait_defn] in
  let typed_method_sigs = List.map ~f:type_method_signature method_sigs in
  let typed_trait_defn = Typed_ast.TTrait (trait_name, typed_method_sigs) in
  Ok typed_trait_defn
   
let type_trait_defns trait_defns  = 
  let%bind () = check_no_duplicate_trait_names trait_defns in
  let%bind () = check_no_duplicate_method_signatures trait_defns in
  Result.all (List.map ~f:type_trait_defn trait_defns) *)
