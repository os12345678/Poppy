open Poppy_parser
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

(* 
1. Field types are valid
2. No duplicate field names
3. Struct name is unique   
*)

let check_no_duplicate_struct_names struct_defns =
  let struct_names = List.map ~f:(fun (Ast.TStruct (struct_name, _, _)) -> struct_name) struct_defns in
  if has_duplicates struct_names ~equal:Ast_types.Struct_name.(=)  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_fields struct_defns =
  let field_defn_lists = List.map ~f:(fun (Ast.TStruct (_, _, field_defn_list)) -> field_defn_list) struct_defns in
  let field_names = List.concat_map ~f:(List.map ~f:(function Ast_types.TField (_, _, field_name, _) -> field_name)) field_defn_lists in
  if has_duplicates field_names ~equal:Ast_types.Field_name.(=)  then
    Or_error.errorf "Duplicate field names found"
  else
    Ok ()

let check_field_types_are_valid struct_defns fields =
  let struct_names = List.map struct_defns ~f:(fun (Ast.TStruct (name, _, _)) -> name) in
  let check_field (Ast_types.TField (_, type_expr, _, _)) =
    match type_expr with
    | Ast_types.TEStruct struct_name ->
      if List.mem struct_names struct_name ~equal:Ast_types.Struct_name.(=) then
        Ok ()
      else
        Error (Base.Error.of_string ("Undefined struct: " ^ Ast_types.Struct_name.to_string struct_name))
    | _ -> Ok ()
  in
  Result.all_unit (List.map fields ~f:check_field)

let type_struct_defn struct_defns (Ast.TStruct (struct_name, capability_list, field_defn_list)) = 
  let%bind () = check_field_types_are_valid struct_defns field_defn_list in
  let typed_struct_defn = Typed_ast.TStruct (struct_name, capability_list, field_defn_list) in
  Ok typed_struct_defn
  
  let type_struct_defns struct_defns = 
    let%bind () = check_no_duplicate_struct_names struct_defns in
    let%bind () = check_no_duplicate_fields struct_defns in
    Result.all (List.map ~f:(type_struct_defn struct_defns) struct_defns)
