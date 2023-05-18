open Core
(* open Poppy_parser.Ast *)
open Poppy_parser.Ast_types

(** 
    [module VarNameMap = Map.Make(Var_name)] creates a new map module [VarNameMap] 
    using [Var_name] as the key. [Map.Make] is a functor that generates a map module
    for a given key type

    [type context = type_expr VarNameMap.t] Define the context as a map from 
    variable names to type expressions 
*)

module VarNameMap = Map.Make(Var_name)
type scope = type_expr VarNameMap.t
type context = scope list

let empty_context : context = []

let push_scope (ctx: context) : context = (* when entering a new scope *)
  VarNameMap.empty :: ctx

let pop_scope (ctx: context) : context = (* when exiting from a scope *)
  match ctx with 
  | _ :: parent_ctx -> parent_ctx
  | [] -> failwith "Cannot pop scope from empty context"

let add_to_context (ctx: context) (v: Var_name.t) (t: type_expr) loc : context =
  match ctx with 
  | [] -> failwith (loc ^ " ::: Cannot add variable " ^ (Var_name.to_string v) ^ " to empty context")
  | scope :: parent_ctx ->
    match VarNameMap.add ~key:v ~data:t scope with
    | `Ok new_scope -> new_scope :: parent_ctx
    | `Duplicate -> failwith (loc ^ " ::: Variable " ^ (Var_name.to_string v) ^ " already exists in context")

let get_var_type (ctx: context) (v: Var_name.t) loc : type_expr option =
  match ctx with 
  | [] -> failwith (loc ^ " ::: Cannot get variable " ^ (Var_name.to_string v) ^ " from empty context")
  | scope :: parent_ctx ->
    match VarNameMap.find scope v with 
    | Some t -> Some t
    | None -> 
      match parent_ctx with (* if not in current scope, check ancestors *)
      | [] -> failwith (loc ^ " ::: Variable " ^ (Var_name.to_string v) ^ " does not exist in context")
      | parent_scope :: _ -> VarNameMap.find parent_scope v

