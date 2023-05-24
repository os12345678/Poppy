open Poppy_parser.Ast_types
(* open Poppy_parser *)
open Core

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

let rec get_var_type (ctx: context) (var: Var_name.t) loc =
  match ctx with
  | [] -> Error (Error.of_string 
                (Fmt.str "%d:%d Type error - Variable %s not defined in environment" (loc.lnum) (loc.cnum) (Var_name.to_string var)))
  | scope :: parent_ctx ->
    match VarNameMap.find scope var with
    | Some t -> Ok t
    | None -> get_var_type parent_ctx var loc

let add_variable (ctx: context) (var: Var_name.t) (t: type_expr) : (context, string) Result.t =
  match ctx with
  | [] -> Error "Cannot add variable to empty context"
  | scope :: parent_ctx ->
    match VarNameMap.add scope ~key:var ~data:t with
    | `Ok new_scope -> Ok (new_scope :: parent_ctx)
    | `Duplicate -> Error ("Variable " ^ Var_name.to_string var ^ " already declared in this scope")
  
(* let rec lookup_field_in_struct (struct_defn: Ast.struct_defn) (var: Var_name.t) (field: Var_name.t) : (type_expr, string) Result.t =
  match List.find struct_defn ~f:(fun s -> s.s_name = var) with
  | Some struct_type ->
    (match List.find struct_type.fields ~f:(fun f -> f.f_name = field) with
    | Some field -> Ok field.f_type
    | None -> Error ("Field " ^ Var_name.to_string field ^ " not found in struct " ^ Var_name.to_string var))
  | None -> Error ("Struct " ^ Var_name.to_string var ^ " not found")
     *)