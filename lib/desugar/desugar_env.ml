open Core

module TAst = Poppy_type_checker.Typed_ast
module T = Poppy_parser.Ast_types

let mangle_name (name: T.Method_name.t) : string =
  "mangled_" ^ T.Method_name.to_string name

let mangle_impl (trait_name: T.Trait_name.t) (struct_name: T.Struct_name.t) (method_name: T.Method_name.t) : string =
  let trait_str = T.Trait_name.to_string trait_name in
  let struct_str = T.Struct_name.to_string struct_name in
  let method_str = T.Method_name.to_string method_name in
  "impl_" ^ trait_str ^ "_for_" ^ struct_str ^ "_" ^ method_str

let mangle_mapp (trait_name: T.Trait_name.t) (struct_name: T.Struct_name.t) (method_name: T.Method_name.t) : string =
    "impl_" ^
    (T.Trait_name.to_string trait_name) ^ 
    "_for_" ^ 
    (T.Struct_name.to_string struct_name) ^ 
    "_" ^ 
    (T.Method_name.to_string method_name) 

let desugar_param (param: T.param) : (T.type_expr * string) =
  match param with
  | Param (type_expr, var_name, _, _) ->
    let name = T.Var_name.to_string var_name in
    (type_expr, name)

(* let get_struct_names (structs: TAst.struct_defn list) : T.Struct_name.t list =
  List.map structs ~f:(fun (TAst.struct_defn (name, _, _, _)) -> name) *)