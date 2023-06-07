open Poppy_parser
(* open Type_env *)
open Core 

(*
trait trait_name {
  fn method_name(param1: type_expr1, param2: type_expr2) -> type_expr3;
}
*)
(* type trait_defn=
| TETrait of
    Trait_name.t
    * Method_name.t
    * borrowed_ref option
    * type_expr
    * param list
  [@@deriving sexp] *)

let check_no_duplicate_trait_names trait_defn =
  let trait_names = List.map ~f:(fun (Ast.TTrait (trait_name, _, _, _, _)) -> trait_name) trait_defn in
  let trait_names_as_strings = List.map ~f:Ast_types.Trait_name.to_string trait_names in
  if List.contains_dup ~compare:String.compare trait_names_as_strings
  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_method_names trait_defn =
  let method_names = List.map ~f:(fun (Ast.TTrait (_, method_name, _, _, _)) -> method_name) trait_defn in
  let method_names_as_strings = List.map ~f:Ast_types.Method_name.to_string method_names in
  if List.contains_dup ~compare:String.compare method_names_as_strings
  then
    Or_error.errorf "Duplicate method signatures found"
  else
    Ok ()

    (* let type_field_defn struct_defn = 
      let check_field (Ast_types.TField (_modifier, type_expr, _field_name, _capability_list)) =
        check_type_valid struct_defn type_expr
      in
      let check_struct (Ast.TStruct (_name, _capability_list, field_defn_list)) =
        List.map ~f:check_field field_defn_list
      in
      List.concat_map ~f:check_struct struct_defn
      |> Result.all_unit *)

    
let type_return_type (Ast.TTrait (_,_,_,type_expr,_))= 
  check_type_valid trait_defn 