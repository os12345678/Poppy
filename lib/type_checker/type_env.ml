open Poppy_parser.Ast_types
open Poppy_parser
open Core
open Core.Result
open Core.Result.Let_syntax

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

let instantiate_this
  (Ast.TTrait (trait_name, _)) =
  (Var_name.of_string "this", TETrait (trait_name, None))

let rec get_var_type (ctx: context) (var: Var_name.t) loc =
  match ctx with
  | [] -> Error (Core.Error.of_string 
                (Fmt.str "%d:%d Type error - Variable %s not defined in environment" (loc.lnum) (loc.cnum) (Var_name.to_string var)))
  | scope :: parent_ctx ->
    match VarNameMap.find scope var with
    | Some t -> Ok t
    | None -> get_var_type parent_ctx var loc

let has_duplicates l ~equal =
  let rec aux seen = function
    | [] -> false
    | h :: t -> if List.exists ~f:(equal h) seen then true else aux (h :: seen) t
  in
  aux [] l

let get_struct_defn struct_name struct_defns loc = 
  let matching_struct_name = 
    List.filter ~f: (fun (Ast.TStruct (name, _, _)) -> Struct_name.(=) name struct_name) struct_defns in
  match matching_struct_name with
  | [] -> Error (Core.Error.of_string 
                (Fmt.str "%s Struct %s is not defined in environment" (string_of_loc loc) (Struct_name.to_string struct_name)))
  | [struct_defn] -> Ok struct_defn
  | _ -> Error  (Core.Error.of_string 
                (Fmt.str "%s Struct %s has multiple definitions with same name" (string_of_loc loc) (Struct_name.to_string struct_name)))

let get_trait_defn trait_name trait_defns loc = 
  let matching_trait_name = 
    List.filter ~f: (fun (Ast.TTrait (name, _)) -> Trait_name.(=) name trait_name) trait_defns in
  match matching_trait_name with
  | [] -> Error (Core.Error.of_string 
                (Fmt.str "%s Trait %s is not defined in environment" (string_of_loc loc) (Trait_name.to_string trait_name)))
  | [trait_defn] -> Ok trait_defn
  | _ -> Error  (Core.Error.of_string 
                (Fmt.str "%s Trait %s has multiple definitions with same name" (string_of_loc loc) (Trait_name.to_string trait_name)))

let get_struct_field field_name (Ast.TStruct(_, _, field_defns)) loc = 
  let matching_field_defns = 
    List.filter ~f: (fun (Ast_types.TField (_, _, name, _)) -> Field_name.(=) name field_name) field_defns in
  match matching_field_defns with 
    | [] -> Error (Core.Error.of_string 
                (Fmt.str "%s Field %s is not defined in struct" (string_of_loc loc) (Field_name.to_string field_name)))
    | [field_defn] -> Ok field_defn
    | _ -> Error  (Core.Error.of_string 
                (Fmt.str "%s Field %s has multiple definitions with same name" (string_of_loc loc) (Field_name.to_string field_name)))

let add_variable (ctx: context) (var: Var_name.t) (t: type_expr) : (context, string) Result.t =
  match ctx with
  | [] -> Error "Cannot add variable to empty context"
  | scope :: parent_ctx ->
    match VarNameMap.add scope ~key:var ~data:t with
    | `Ok new_scope -> Ok (new_scope :: parent_ctx)
    | `Duplicate -> Error ("Variable " ^ Var_name.to_string var ^ " already declared in this scope")

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

let is_this var_name loc = 
  if phys_equal var_name (Var_name.of_string "this") then 
    Error (Core.Error.of_string 
            (Fmt.str "%d:%d Type error - Cannot use 'this' in this context" (loc.lnum) (loc.cnum)))
  else Ok ()

let is_modifier_const = function
| Ast_types.MConst -> true
| _ -> false

let identifier_assignable id struct_defns context loc = 
  match id with 
  | Ast.Variable var_name -> is_this var_name loc
  | Ast.ObjField (obj_name, field_name) -> 
    let%bind struct_defn = get_obj_struct_defn obj_name struct_defns context loc in 
    let%bind (Ast_types.TField(modifier, _, _, _)) = get_struct_field field_name struct_defn loc in
    if phys_equal modifier (Ast_types.MConst) then
      Error (Core.Error.of_string 
              (Fmt.str "%d:%d Type error - Cannot assign to const field %s" (loc.lnum) (loc.cnum) (Field_name.to_string field_name)))
    else Ok ()


let check_type_valid trait_defn type_expr error_prefix = 
  match type_expr with
  | TEBool | TEInt | TEVoid -> Ok ()
  | TETrait (trait_name, _) -> (
    get_trait_defn trait_name trait_defn (loc_of_position Lexing.dummy_pos)
    |> function 
      | Ok _ -> Ok ()
      | Error _ -> 
        Error
          (Core.Error.of_string
             (Fmt.str "%s - trait %s doesn't exists" error_prefix
                (Trait_name.to_string trait_name))) )
  | TEStruct _ -> Error (Core.Error.of_string (Fmt.str "%s - struct type not implemented" error_prefix))



let get_matching_trait_defn var_name trait_defns context loc = 
  let open Result in
  get_var_type context var_name loc 
    >>= function 
    | Ast_types.TETrait (trait_name, _) ->
      get_trait_defn trait_name trait_defns loc
      >>| fun var_name -> (var_name)
    | wrong_type -> Error (Core.Error.of_string 
                  (Fmt.str "%s Type error - %s should be a trait, instead is of type %s" 
                  (string_of_loc loc) 
                  (Var_name.to_string var_name)
                  (string_of_type wrong_type)))

(* This function retrieves the method definition for a given method name from a list of trait definitions *)
let get_method_defn method_name trait_defns loc =
  (* First, flatten the list of method signatures from all trait definitions *)
  let all_method_signatures = List.concat_map ~f:(fun defn -> 
    match defn with
    | Ast.TTrait (_, signatures) -> signatures) trait_defns in
  (* Then, find the method signature with the given method name *)
  match List.find ~f:(fun (TMethodSignature (name, _, _, _, _)) -> Method_name.(=) name method_name) all_method_signatures with
  | None -> Or_error.error_string 
                (Fmt.str "Method %s not found at %s" (Method_name.to_string method_name) (string_of_loc loc))
  | Some method_signature -> match method_signature with 
    | TMethodSignature (_, _, _, _, method_ret_type) -> Ok method_ret_type
    
(* This function retrieves the trait definitions for a given struct *)
let get_struct_trait_defns struct_defn method_defns trait_defns loc =
  let struct_name = match struct_defn with
    | Ast.TStruct (name, _, _) -> name in
  (* First, find all the method definitions for the given struct *)
  let methods_for_struct = List.filter ~f:(fun defn -> match defn with
                                                      | Ast.TMethod (_, Some name, _, _) -> Struct_name.(=) name struct_name
                                                      | _ -> false) method_defns in
  (* Then, extract the trait names from these method definitions *)
  let trait_names = List.map ~f:(fun (TMethod (trait_name, _, _, _)) -> trait_name) methods_for_struct in
  (* Finally, find the trait definitions corresponding to these trait names *)
  let trait_defns_for_struct = List.filter ~f:(fun (Ast.TTrait (trait_name, _)) -> List.mem trait_names trait_name ~equal:Trait_name.(=)) trait_defns in
  if List.is_empty trait_defns_for_struct then
    Or_error.error_string 
        (Fmt.str "No traits found for struct %s at %s" (Struct_name.to_string struct_name) (string_of_loc loc))
  else
    Ok trait_defns_for_struct
    

(* let check_argument_types (Ast.TMethod (_, _, TMethodSignature (_, _, _, params, _), _)) typed_args loc =
  let expected_types = List.map ~f:type_of_param params in
  let arg_types = List.map ~f:(fun { Typed_ast.typ; _ } -> typ) typed_args in
  if List.equal ~equal:Type.equal expected_types arg_types then
    Ok ()
  else
    Or_error.error_string 
        (Fmt.str "Type error at %s - Argument types do not match method signature" (string_of_loc loc))
     *)


(* let check_method_exists_for_struct struct_defn method_defn loc =
  match struct_defn, method_defn with
  | Ast.TStruct (struct_name, _, _), Ast.TMethod (trait_name, _, _, _) ->
    if List.exists ~f:(fun trait -> Trait_name.(=) trait trait_name) struct_defn.traits then
      Ok ()
    else
      Error (Error.of_string (Fmt.str "%s Method %s does not exist for struct %s" (string_of_loc loc) (Method_name.to_string method_defn.method_name) (Struct_name.to_string struct_name)))
  | _ -> Error (Error.of_string (Fmt.str "%s Type error - check_method_exists_for_struct received wrong types" (string_of_loc loc)))
                 *)

(* find matching function return type *)
let get_matching_function_type func_name function_defns loc = 
  let matching_func_name = 
    List.filter ~f: (fun (Ast.TFunction (name, _, _, _, _)) -> Function_name.(=) name func_name) function_defns in
  match matching_func_name with
  | [] -> Error (Core.Error.of_string 
                (Fmt.str "%s Function %s is not defined in environment" (string_of_loc loc) (Function_name.to_string func_name)))
  | [Ast.TFunction (_, _, return_type, _, _)] -> Ok return_type
  | _ -> Error  (Core.Error.of_string 
                (Fmt.str "%s Function %s has multiple definitions with same name" (string_of_loc loc) (Function_name.to_string func_name)))

let get_matching_method_type method_name method_defns loc = 
  let matching_method_name = 
    List.filter ~f: (fun (Ast.TMethodSignature (name, _, _, _, _)) -> Method_name.(=) name method_name) method_defns in
  match matching_method_name with
  | [] -> Error (Core.Error.of_string 
                (Fmt.str "%s Method %s is not defined in " (string_of_loc loc) (Method_name.to_string method_name)))
  | [Ast.TMethodSignature (_, _, _, _, return_type)] -> Ok return_type
  | _ -> Error  (Core.Error.of_string 
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

  let check_no_duplicate_var_declarations_in_block (exprs: Ast.expr list) loc =
    if List.contains_dup ~compare: (
      fun expr1 expr2 ->
        match expr1.node with 
        | Ast.Let (_, var_name1, _) -> 
          (match expr2.node with 
          | Ast.Let (_, var_name2, _) -> 
            if Var_name.(=) var_name1 var_name2 then 0 else 1 
          | _ -> 1)
        | _ -> 1) exprs then
      Error (Core.Error.of_string 
              (Fmt.str "%s Type error - Duplicate variable declarations in same block" (string_of_loc loc)))
    else Ok ()
  
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
  