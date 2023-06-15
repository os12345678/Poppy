open Poppy_parser
open Type_env
open Core 

(* Check for trait invariances *)

let check_no_duplicate_trait_names trait_defn =
  let trait_names = List.map ~f:(fun (Ast.TTrait (trait_name, _)) -> trait_name) trait_defn in
  if has_duplicates trait_names ~equal:Ast_types.Trait_name.(=)  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_method_signatures trait_defn =
  let method_signature = List.map ~f:(fun (Ast.TTrait (_, method_signatures)) -> method_signatures) trait_defn in
  let method_signature_names = List.concat_map ~f:(List.map ~f:(function Ast.TMethodSignature (method_name, _, _, _, _) -> method_name)) method_signature in
  if has_duplicates method_signature_names ~equal:Ast_types.Method_name.(=)  then
    Or_error.errorf "Duplicate field names found"
  else
    Ok ()

(* Type check trait body *)

let init_env_from_params params =
  let param_pairs = List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params in
  match VarNameMap.of_alist param_pairs with
  | `Duplicate_key _ -> Error (Base.Error.of_string "Duplicate parameter names found")
  | `Ok map -> Ok [map]

(* TODO type check method signatures *)

(* let type_check_method_signatures trait_defns method_name  *) 

(* let type_trait_defn *)

(* let type_trait_defns *)

