(* open Poppy_parser.Ast_types
open Poppy_parser
open Core
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
    let method_strs = List.map ~f:(fun (Ast.TMethodSignature (name, _, _, _, _)) -> 
      (Method_name.to_string name)) methods in
    "trait " ^ (Trait_name.to_string name) ^ " {" ^ (String.concat ~sep:", " method_strs) ^ "}"

let method_defn_to_string method_defn =
  match method_defn with
  | Ast.TMethod (_, _, (Ast.TMethodSignature(name, _, _, _, _)), _) ->
    "method " ^ (Method_name.to_string name)

let function_defn_to_string function_defn =
  match function_defn with
  | Ast.TFunction (name, _, _, _, _) ->
    "function " ^ (Function_name.to_string name) *)
