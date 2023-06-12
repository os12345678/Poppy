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

let get_struct_defn struct_name struct_defns loc = 
  let matching_struct_name = 
    List.filter ~f: (fun (Ast.TStruct (name, _, _)) -> Struct_name.(=) name struct_name) struct_defns in
  match matching_struct_name with
  | [] -> Error (Error.of_string 
                (Fmt.str "%s Struct %s is not defined in environment" (string_of_loc loc) (Struct_name.to_string struct_name)))
  | [struct_defn] -> Ok struct_defn
  | _ -> Error  (Error.of_string 
                (Fmt.str "%s Struct %s has multiple definitions with same name" (string_of_loc loc) (Struct_name.to_string struct_name)))

let get_trait_defn trait_name trait_defns loc = 
  let matching_trait_name = 
    List.filter ~f: (fun (Ast.TTrait (name, _)) -> Trait_name.(=) name trait_name) trait_defns in
  match matching_trait_name with
  | [] -> Error (Error.of_string 
                (Fmt.str "%s Trait %s is not defined in environment" (string_of_loc loc) (Trait_name.to_string trait_name)))
  | [trait_defn] -> Ok trait_defn
  | _ -> Error  (Error.of_string 
                (Fmt.str "%s Trait %s has multiple definitions with same name" (string_of_loc loc) (Trait_name.to_string trait_name)))

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

let get_obj_struct_defn var_name struct_names context loc = 
  let open Result in 
  get_var_type context var_name loc 
    >>= function 
    | Ast_types.TEStruct (struct_name) ->
      get_struct_defn struct_name struct_names loc
      >>| fun var_name -> (var_name)
    | wrong_type -> Error (Core.Error.of_string 
                  (Fmt.str "%s Type error - %s should be an object, instead is of type %s" 
                  (string_of_loc loc) 
                  (Var_name.to_string var_name)
                  (string_of_type wrong_type)))

let get_matching_trait_defn var_name trait_defns context loc = 
  let open Result in
  get_var_type context var_name loc 
    >>= function 
    | Ast_types.TETrait (trait_name) ->
      get_trait_defn trait_name trait_defns loc
      >>| fun var_name -> (var_name)
    | wrong_type -> Error (Core.Error.of_string 
                  (Fmt.str "%s Type error - %s should be a trait, instead is of type %s" 
                  (string_of_loc loc) 
                  (Var_name.to_string var_name)
                  (string_of_type wrong_type)))

(* find matching function return type *)
let get_matching_function_type func_name function_defns loc = 
  let matching_func_name = 
    List.filter ~f: (fun (Ast.TFunction (name, _, _, _, _)) -> Function_name.(=) name func_name) function_defns in
  match matching_func_name with
  | [] -> Error (Error.of_string 
                (Fmt.str "%s Function %s is not defined in environment" (string_of_loc loc) (Function_name.to_string func_name)))
  | [Ast.TFunction (_, _, return_type, _, _)] -> Ok return_type
  | _ -> Error  (Error.of_string 
                (Fmt.str "%s Function %s has multiple definitions with same name" (string_of_loc loc) (Function_name.to_string func_name)))

let get_matching_method_type method_name method_defns loc = 
  let matching_method_name = 
    List.filter ~f: (fun (Ast.TMethodSignature (name, _, _, _, _)) -> Method_name.(=) name method_name) method_defns in
  match matching_method_name with
  | [] -> Error (Error.of_string 
                (Fmt.str "%s Method %s is not defined in " (string_of_loc loc) (Method_name.to_string method_name)))
  | [Ast.TMethodSignature (_, _, _, _, return_type)] -> Ok return_type
  | _ -> Error  (Error.of_string 
                (Fmt.str "%s Method %s has multiple definitions with same name" (string_of_loc loc) (Method_name.to_string method_name)))

let get_struct_field field_name _struct_defns 
    (Ast.TStruct(_, _, fields)) loc =
    let open Result in 
    let matching_field_name = 
      List.filter ~f: (fun (Ast_types.TField (_, _type_expr, name, _caps)) -> Field_name.(=) name field_name) fields in
    match matching_field_name with
    | [] -> Error (Core.Error.of_string 
                  (Fmt.str "%s Field %s is not defined in struct" (string_of_loc loc) (Field_name.to_string field_name)))
    | [Ast_types.TField (_, type_expr, _, _)] -> Ok type_expr
    | _ -> Error  (Core.Error.of_string 
                  (Fmt.str "%s Field %s has multiple definitions with same name" (string_of_loc loc) (Field_name.to_string field_name)))


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
  