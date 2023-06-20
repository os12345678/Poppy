open Poppy_parser.Ast_types
open Poppy_parser
open Core
open Core.Result
open Core.Result.Let_syntax

(* Environment *)
module VarNameMap = Map.Make(Var_name)
module StructNameMap = Map.Make(Struct_name)
module FunctionNameMap = Map.Make(Function_name)
module MethodNameMap = Map.Make(Method_name)
module TraitNameMap = Map.Make(Trait_name)

type env = 
  | Global of Ast.struct_defn StructNameMap.t * Ast.trait_defn TraitNameMap.t 
    * Ast.method_defn MethodNameMap.t * Ast.function_defn FunctionNameMap.t
  | Function of env * type_expr VarNameMap.t
  | Block of env * type_expr VarNameMap.t
  [@@deriving sexp]

let init_global_scope () =
  Global (StructNameMap.empty, TraitNameMap.empty, MethodNameMap.empty, FunctionNameMap.empty)

(* Lookup functions *)
let lookup_struct env struct_name =
  match env with
  | Global (struct_map, _, _, _) -> 
    begin
      match StructNameMap.find struct_map struct_name with
      | Some struct_defn -> Ok struct_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Struct %s not found" (Struct_name.to_string struct_name)))
    end
  | _ -> Error (Core.Error.of_string (Fmt.str "%s not in global scope" (Struct_name.to_string struct_name)))

let lookup_trait env trait_name =
  match env with
  | Global (_, trait_map, _, _) -> 
    begin
      match TraitNameMap.find trait_map trait_name with
      | Some trait_defn -> Ok trait_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Trait %s not found" (Trait_name.to_string trait_name)))
    end
  | _ -> Error (Core.Error.of_string "Not in global scope")

let lookup_method env method_name =
  match env with
  | Global (_, _, method_map, _) -> 
    begin
      match MethodNameMap.find method_map method_name with
      | Some method_defn -> Ok method_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Method %s not found" (Method_name.to_string method_name)))
    end
  | _ -> Error (Core.Error.of_string "Not in global scope")

let lookup_function env function_name =
  match env with
  | Global (_, _, _, function_map) -> 
    begin
      match FunctionNameMap.find function_map function_name with
      | Some function_defn -> Ok function_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Function %s not found" (Function_name.to_string function_name)))
    end
  | _ -> Error (Core.Error.of_string "Not in global scope")

let rec lookup_var env var_name =
  match env with
  | Global _ -> Error (Core.Error.of_string (Fmt.str "Variable %s not found" (Var_name.to_string var_name)))
  | Function (parent_env, var_map) | Block (parent_env, var_map) ->
    begin 
      match VarNameMap.find var_map var_name with
      | Some var_type -> Ok var_type
      | None -> lookup_var parent_env var_name
    end

let rec find_global env =
  match env with
  | Global _ -> env
  | Function (parent_env, _) | Block (parent_env, _) -> find_global parent_env
    

(* Add Functions *)
let add_struct_to_global env struct_defn =
  match env, struct_defn with
  | Global (struct_map, trait_map, method_map, function_map), Ast.TStruct (name, _, _) ->
      let new_struct_map = StructNameMap.add_exn struct_map ~key:name ~data:struct_defn in
      Global (new_struct_map, trait_map, method_map, function_map)
  | _ -> env

let add_trait_to_global env trait_defn =
  match env, trait_defn with
  | Global (struct_map, trait_map, method_map, function_map), Ast.TTrait (name, _) ->
      let new_trait_map = TraitNameMap.add_exn trait_map ~key:name ~data:trait_defn in
      Global (struct_map, new_trait_map, method_map, function_map)
  | _ -> env

let add_method_to_global env method_defn =
  match env, method_defn with
  | Global (struct_map, trait_map, method_map, function_map), Ast.TMethod (_, _, TMethodSignature (method_name, _, _, _, _), _) ->
      let new_method_map = MethodNameMap.add_exn method_map ~key:method_name ~data:method_defn in
      Global (struct_map, trait_map, new_method_map, function_map)
  | _ -> env

let add_function_to_global env function_defn =
  match env, function_defn with
  | Global (struct_map, trait_map, method_map, function_map), Ast.TFunction (function_name, _, _, _, _) ->
      let new_function_map = FunctionNameMap.add_exn function_map ~key:function_name ~data:function_defn in
      Global (struct_map, trait_map, method_map, new_function_map)
  | _ -> env

let add_function_scope env params =
  Function (env, params)

let add_block_scope env var_map = Block (find_global env, var_map)


let add_var_to_block_scope env var_name var_type =
  match env with
  | Block (parent, params) ->
    let new_params = VarNameMap.add_exn params ~key:var_name ~data:var_type in
    Block (parent, new_params)
  | _ -> env

(* Remove Functions *)
let remove_scope = function
| Global _ -> Error (Base.Error.of_string "Cannot remove global scope")
| Function (parent, _) -> Ok parent
| Block (parent, _) -> Ok parent

(* Getter Functions *)
let get_obj_struct_defn var_name env loc = 
  lookup_var env var_name
  >>= function
  | Ast_types.TEStruct (struct_name) ->
      lookup_struct env struct_name
      >>| fun var_name -> var_name
  | wrong_type -> Error (Core.Error.of_string 
                  (Fmt.str "%s Type error - %s should be an object, instead is of type %s" 
                  (string_of_loc loc) 
                  (Var_name.to_string var_name)
                  (string_of_type wrong_type)))
  
(* Invariances *)
let has_duplicates l ~equal =
  let rec aux seen = function
    | [] -> false
    | h :: t -> if List.exists ~f:(equal h) seen then true else aux (h :: seen) t
  in
  aux [] l

let check_no_duplicate_var_declarations_in_block exprs loc = 
  let var_names = List.filter_map ~f:(function Ast.{node = Let (_ , name, _); _} -> Some name | _ -> None) exprs in
  if has_duplicates var_names ~equal:Var_name.(=) then
    Error (Core.Error.of_string 
      (Fmt.str "%d:%d Type error - Duplicate variable declaration in block" (loc.lnum) (loc.cnum)))
  else
    Ok ()

(* Assignability *)
let check_variable_declarable var_name loc = 
  if phys_equal var_name (Var_name.of_string "this") then 
    Error (Core.Error.of_string 
            (Fmt.str "%d:%d Type error - Cannot use 'this' in this context" (loc.lnum) (loc.cnum)))
  else Ok ()

let check_identifier_assignable id env loc = 
  match id with 
  | Ast.Variable var_name -> check_variable_declarable var_name loc
  | Ast.ObjField (obj_name, field_name) -> 
    let%bind (Ast.TStruct (_, _, fields)) = get_obj_struct_defn obj_name env loc in
    match List.find ~f:(fun (Ast_types.TField (_, _, name, _)) -> Field_name.(=) name field_name) fields with
    | Some (Ast_types.TField(modifier, _, _, _)) ->
      if phys_equal modifier (Ast_types.MConst) then
        Error (Core.Error.of_string 
                (Fmt.str "%d:%d Type error - Cannot assign to const field %s" (loc.lnum) (loc.cnum) (Field_name.to_string field_name)))
      else Ok ()
    | None -> Error (Core.Error.of_string 
                (Fmt.str "%d:%d Type error - Field %s not found in struct" (loc.lnum) (loc.cnum) (Field_name.to_string field_name)))
              
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
    "function " ^ (Function_name.to_string name)

let rec print_global_env env =
  match env with
  | Global (struct_map, trait_map, method_map, function_map) ->
    let struct_strs = StructNameMap.to_alist struct_map |> List.map ~f:(fun (name, defn) -> Struct_name.to_string name ^ ": " ^ struct_defn_to_string defn) in
    let trait_strs = TraitNameMap.to_alist trait_map |> List.map ~f:(fun (name, defn) -> Trait_name.to_string name ^ ": " ^ trait_defn_to_string defn) in
    let method_strs = MethodNameMap.to_alist method_map |> List.map ~f:(fun (name, defn) -> Method_name.to_string name ^ ": " ^ method_defn_to_string defn) in
    let function_strs = FunctionNameMap.to_alist function_map |> List.map ~f:(fun (name, defn) -> Function_name.to_string name ^ ": " ^ function_defn_to_string defn) in
    print_endline "\nGlobal Environment:";
    print_endline "~~~~~Structs~~~~~";
    List.iter ~f:print_endline struct_strs;
    print_endline "~~~~~Traits~~~~~";
    List.iter ~f:print_endline trait_strs;
    print_endline "~~~~~Methods~~~~~";
    List.iter ~f:print_endline method_strs;
    print_endline "~~~~~Functions~~~~~";
    List.iter ~f:print_endline function_strs;
  | Function (env, _) -> 
    print_endline "\nFunction Environment:";
    print_global_env env
  | Block (env, _) -> 
    print_endline "\nBlock Environment:";
    print_global_env env


                  

(* let push_scope (ctx: context) : context = (* when entering a new scope *)
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


  let check_field_types_are_valid struct_defns fields =
    let struct_names = List.map struct_defns ~f:(fun (Ast.TStruct (name, _, _)) -> name) in
    let check_field (Ast_types.TField (_, type_expr, _, _)) =
      match type_expr with
      | Ast_types.TEStruct struct_name ->
        if List.mem struct_names struct_name ~equal:Struct_name.(=) then
          Ok ()
        else
          Error (Base.Error.of_string ("Undefined struct: " ^ Struct_name.to_string struct_name))
      | _ -> Ok ()
    in
    Result.all_unit (List.map fields ~f:check_field)

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
                                                      | Ast.TMethod (_, name, _, _) -> Struct_name.(=) name struct_name) method_defns in
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
   *)