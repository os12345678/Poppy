open Poppy_parser
open Type_env
open Core 

(* Check struct invariances *)

let check_no_duplicate_struct_names struct_defn =
  let struct_names = List.map ~f:(fun (Ast.TStruct (struct_name, _, _)) -> struct_name) struct_defn in
  if has_duplicates struct_names ~equal:Ast_types.Struct_name.(=)  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_fields struct_defn =
  let field_defn_lists = List.map ~f:(fun (Ast.TStruct (_, _, field_defn_list)) -> field_defn_list) struct_defn in
  let field_names = List.concat_map ~f:(List.map ~f:(function Ast_types.TField (_, _, field_name, _) -> field_name)) field_defn_lists in
  if has_duplicates field_names ~equal:Ast_types.Field_name.(=)  then
    Or_error.errorf "Duplicate field names found"
  else
    Ok ()

let type_field_defn struct_defn = 
  let check_field (Ast_types.TField (_modifier, type_expr, _field_name, _capability_list)) =
    check_type_valid struct_defn type_expr
  in
  let check_struct (Ast.TStruct (_, _, field_defn_list)) =
    List.map ~f:check_field field_defn_list
  in
  List.concat_map ~f:check_struct struct_defn
  |> Result.all_unit

let init_env_from_params params =
  let param_pairs = List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params in
  match VarNameMap.of_alist param_pairs with
  | `Duplicate_key _ -> Error (Base.Error.of_string "Duplicate parameter names found")
  | `Ok map -> Ok [map]

let type_struct_defn (Ast.TStruct (struct_name, capability_list, field_defn_list) as current_struct_defn) = 
  let open Result in
  let open Core.Result.Let_syntax in
  let%bind () = check_no_duplicate_struct_names [current_struct_defn] in
    let%bind () = check_no_duplicate_fields [current_struct_defn] in
      let%bind () = type_field_defn [current_struct_defn] in
  let typed_struct_defn = Typed_ast.TStruct (struct_name, capability_list, field_defn_list) in
  Ok typed_struct_defn
  
let type_struct_defns struct_defns = 
  let open Core.Result.Let_syntax in
  let%bind () = check_no_duplicate_struct_names struct_defns in
    let%bind () = check_no_duplicate_fields struct_defns in
  Result.all (List.map ~f:type_struct_defn struct_defns)
    