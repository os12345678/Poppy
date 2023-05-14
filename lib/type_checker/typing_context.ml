open Core
(* open Poppy_parser.Ast *)
open Poppy_parser.Ast_types

(** 
    [module VarNameMap = Map.Make(Var_name)] creates a new map module [VarNameMap] 
    using [Var_name] as the key. [Map.Make] is a functor that generates a map module
    for a given key type

    [type context = type_expr VarNameMap.t] defines a new type context which is a 
    map from [Var_name.t] to [type_expr]. Each entry in the context map represents 
    a variable in the current scope of the program being type-checked. The key is 
    the name of the variable and the value is the type of the variable.
*)

module VarNameMap = Map.Make(Var_name)
type context = type_expr VarNameMap.t

let get_var_type (ctx: context) (v: Var_name.t) loc: type_expr =
  match VarNameMap.find ctx v with
  | Some t -> t 
  | None -> failwith (
    string_of_loc(loc_of_position loc) ^ " ::: [Type Error] Variable " ^ (Var_name.to_string v) ^ " not found in context")
