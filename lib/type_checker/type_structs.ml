(* open Core 
open Poppy_parser
(* open Type_env *)

(* Check for duplicate definitions *)
let check_no_duplicate_struct_names struct_defns =
  let names = List.filter_map struct_defns ~f:(function
      | Ast_types.TEStruct(name,_) -> Some (Ast_types.Struct_name.to_string name)
      | _ -> None
  ) in
  let name_set = String.Set.of_list names in
  if List.length names <> Set.length name_set then
    Ok ()
  else
    Error 
      (Error.of_string 
      (Fmt.str "Duplicate struct names found"))

let check_no_duplicate_fields field_defns = 
  let names = List.filter_map field_defns ~f:(function
      | Ast_types.TField(_,_,name,_) -> Some (Ast_types.Field_name.to_string name)
  ) in
  let name_set = String.Set.of_list names in
  if List.length names <> Set.length name_set then
    Ok ()
  else
    Error 
      (Error.of_string 
      (Fmt.str "Duplicate field names found" ^ (String.concat ~sep:", " names)))

Type check definitions and signatures *)
