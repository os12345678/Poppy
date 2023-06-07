open Poppy_parser.Ast_types
open Poppy_parser
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

let is_this var_name loc = 
  if phys_equal var_name (Var_name.of_string "this") then 
    Error (Error.of_string 
            (Fmt.str "%d:%d Type error - Cannot use 'this' in this context" (loc.lnum) (loc.cnum)))
  else Ok ()

let identifier_assignable id loc = 
  match id with 
  | Ast.Variable var_name -> is_this var_name loc
  | Ast.ObjField (_, _) -> Or_error.error_string "Identifier assignable for ObjField not implemented"

let check_type_valid struct_defn_list type_expr = 
  match type_expr with
  | TEBool | TEInt | TEVoid -> Ok ()
  | TEStruct struct_name -> 
    let struct_names = List.map ~f:(function Ast.TStruct (name, _, _) -> name) struct_defn_list in
    if List.mem struct_names struct_name ~equal:Struct_name.(=) then Ok ()
    else Or_error.error_string "Type not defined"
  | TETrait _trait_name -> Or_error.error_string "Trait type not implemented"
  
  (* let check_no_duplicate_var_declarations_in_block (exprs: Ast.expr list) (loc: loc) : (unit, string) Result.t =
    let open Result in
    let var_set = VarNameMap.empty in
    let rec check_duplicates exprs var_set =
      match exprs with
      | [] -> Ok ()
      | expr :: rest ->
        match expr with
        | Ast.Let (_, var_name, _) ->
          if VarNameMap.mem var_set var_name then
            Error (Fmt.str "%d:%d Type error - Variable %s already declared in this block" loc.lnum loc.cnum (Var_name.to_string var_name))
          else
            let updated_set = VarNameMap.add var_set var_name in
            check_duplicates rest updated_set
        | _ -> check_duplicates rest var_set
    in
    check_duplicates exprs var_set
   *)

let rec print_context (ctx: context) : unit =
  match ctx with
  | [] -> ()
  | scope :: parent_ctx ->
    print_scope scope;
    print_context parent_ctx

and print_scope (scope: scope) : unit =
  VarNameMap.iteri ~f:(fun ~key:var ~data:t ->
    let t_as_sexp = Sexplib.Sexp.to_string_hum (sexp_of_type_expr t) in
    Printf.printf "%s: %s\n" (Var_name.to_string var) t_as_sexp) scope
  