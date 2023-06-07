open Poppy_parser
open Type_env
open Core 

let check_no_duplicate_struct_names struct_defn =
  let struct_names = List.map ~f:(fun (Ast.TStruct (struct_name, _, _)) -> struct_name) struct_defn in
  let struct_names_as_strings = List.map ~f:Ast_types.Struct_name.to_string struct_names in
  if List.contains_dup ~compare:String.compare struct_names_as_strings
  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_fields struct_defn =
  let field_defn_lists = List.map ~f:(fun (Ast.TStruct (_, _, field_defn_list)) -> field_defn_list) struct_defn in
  let field_names = List.concat_map ~f:(List.map ~f:(function Ast_types.TField (_, _, field_name, _) -> field_name)) field_defn_lists in
  let field_names_as_strings = List.map ~f:Ast_types.Field_name.to_string field_names in
  if List.contains_dup ~compare:String.compare field_names_as_strings
  then
    Or_error.errorf "Duplicate field names found"
  else
    Ok ()

let type_field_defn struct_defn = 
  let check_field (Ast_types.TField (_modifier, type_expr, _field_name, _capability_list)) =
    check_type_valid struct_defn type_expr
  in
  let check_struct (Ast.TStruct (_name, _capability_list, field_defn_list)) =
    List.map ~f:check_field field_defn_list
  in
  List.concat_map ~f:check_struct struct_defn
  |> Result.all_unit

(* TODO: type capabilities in struct body *)
    
(* 
struct struct_name {
  field_name1: type_expr1,
  field_name2: type_expr2,
} 
*)
(* type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list
  * field_defn list
  [@@deriving sexp] *)

  let type_struct_defn (Ast.TStruct (struct_name, capability_list, field_defn_list) as current_struct_defn) 
  : (Typed_ast.struct_defn, Error.t) Result.t= 
    let open Result in
    check_no_duplicate_struct_names [current_struct_defn] >>= fun _ ->
    check_no_duplicate_fields [current_struct_defn] >>= fun _ ->
    type_field_defn [current_struct_defn] >>= fun _ ->
    let typed_struct_defn = Typed_ast.TStruct (struct_name, capability_list, field_defn_list) in
    Ok typed_struct_defn
  
let type_struct_defns struct_defns = 
  let open Result in
  check_no_duplicate_struct_names struct_defns >>= fun _ ->
  check_no_duplicate_fields struct_defns >>= fun _ ->
  Result.all (List.map ~f:type_struct_defn struct_defns)
    