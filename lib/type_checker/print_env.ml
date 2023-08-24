open Poppy_parser.Ast_types
open Poppy_parser
open Core
open Type_env
(* open Core.Result
open Core.Result.Let_syntax *)

(* Print Functions *)
let struct_defn_to_string struct_defn = 
  match struct_defn with
  | Ast.TStruct (name, _, fields) -> 
    let field_strs = List.map ~f:(fun (Ast_types.TField (modifier, typ, name, _)) -> 
      (Field_name.to_string name) ^ ": " ^ (string_of_type typ) ^ " " ^ (string_of_modifier modifier)) fields in
    "struct " ^ (Struct_name.to_string name) ^ " {" ^ (String.concat ~sep:", " field_strs) ^ "}"

let trait_defn_to_string trait_defn =
  match trait_defn with
  | Ast.TTrait (name, methods) ->
    let method_strs = List.map ~f:(fun method_signature -> 
      (Method_name.to_string method_signature.name)) methods in
    "trait " ^ (Trait_name.to_string name) ^ " {" ^ (String.concat ~sep:", " method_strs) ^ "}"
    
let method_defn_to_string method_defn =
  match method_defn with
  | Ast.TMethod (method_signature, _) ->
    "method " ^ (Method_name.to_string method_signature.name)

let function_defn_to_string function_defn =
  match function_defn with
  | Ast.TFunction (function_signature, _) ->
    "function " ^ (Function_name.to_string function_signature.name)

let rec print_env env =
  match env with
  | Global (struct_map, trait_map, method_map, function_map, struct_trait_map) ->
    print_endline "Global scope:";
    print_endline "Structs:";
    StructNameMap.iteri struct_map ~f:(fun ~key ~data->
      print_endline (Struct_name.to_string key);
      print_endline (struct_defn_to_string data));
    print_endline "Traits:";
    TraitNameMap.iteri trait_map ~f:(fun ~key ~data ->
      print_endline (Trait_name.to_string key);
      print_endline (trait_defn_to_string data));
    print_endline "Methods:";
    MethodNameMap.iteri method_map ~f:(fun ~key ~data ->
      print_endline (Method_name.to_string key);
      print_endline (method_defn_to_string data));
    print_endline "Functions:";
    FunctionNameMap.iteri function_map ~f:(fun ~key ~data ->
      print_endline (Function_name.to_string key);
      print_endline (function_defn_to_string data));
    print_endline "Struct-Trait Map:";
    StructTraitMap.iteri struct_trait_map ~f:(fun ~key ~data ->
      print_endline (Struct_name.to_string key ^ ": " ^ (String.concat ~sep:", " (List.map ~f:Trait_name.to_string data))));
  | Function (parent_env, var_map) ->
    print_endline "Function scope:";
    print_env parent_env;
    print_endline "Variables:";
    VarNameMap.iteri var_map ~f:(fun ~key ~data ->
      print_endline (Var_name.to_string key);
      print_endline (Ast_types.string_of_type data));
  | Block (parent_env, var_map) ->
    print_endline "Block scope:";
    print_env parent_env;
    print_endline "Variables:";
    VarNameMap.iteri var_map ~f:(fun ~key ~data ->
      print_endline (Var_name.to_string key);
      print_endline (Ast_types.string_of_type data));;

let print_block_scope env =
  match env with
  | Block (_, var_map) ->
    print_endline "Block scope:";
    VarNameMap.iteri var_map ~f:(fun ~key ~data ->
      print_endline ("\t"^Var_name.to_string key^": "^Ast_types.string_of_type data));
  | _ -> print_endline "Nothing to show"
      