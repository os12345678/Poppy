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
module StructTraitMap = Map.Make(Struct_name)

type env = 
  | Global of Ast.struct_defn StructNameMap.t * Ast.trait_defn TraitNameMap.t 
    * Ast.method_defn MethodNameMap.t * Ast.function_defn FunctionNameMap.t
    * Trait_name.t list StructTraitMap.t (* method implementations *)
  | Function of env * type_expr VarNameMap.t
  | Block of env * type_expr VarNameMap.t
  [@@deriving sexp]

let init_global_scope () =
  Global (StructNameMap.empty, TraitNameMap.empty, MethodNameMap.empty, FunctionNameMap.empty, StructTraitMap.empty)

let rec equal_type_expr_list list1 list2 =
  match list1, list2 with
  | [], [] -> true
  | te1 :: rest1, te2 :: rest2 when equal_type_expr te1 te2 -> equal_type_expr_list rest1 rest2
  | _ -> false

and equal_type_expr te1 te2 =
  match te1, te2 with
  | TEInt, TEInt -> true
  | TEBool, TEBool -> true
  | TEStruct name1 , TEStruct name2 -> Struct_name.(=) name1 name2
  | TETrait (name1, _), TETrait (name2, _) -> Trait_name.(=) name1 name2
  | _ -> false
  

(* Lookup functions *)
let rec find_global env =
  match env with
  | Global _ -> env
  | Function (parent_env, _) | Block (parent_env, _) -> find_global parent_env

let rec lookup_struct env struct_name =
  match env with
  | Global (struct_map, _, _, _, _) -> 
    begin
      match StructNameMap.find struct_map struct_name with
      | Some struct_defn -> Ok struct_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Struct %s not found" (Struct_name.to_string struct_name)))
    end
  | Function (parent_env, _) | Block (parent_env, _) -> lookup_struct parent_env struct_name

let rec lookup_trait env trait_name =
  match env with
  | Global (_, trait_map, _, _, _) -> 
    begin
      match TraitNameMap.find trait_map trait_name with
      | Some trait_defn -> Ok trait_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Trait %s not found" (Trait_name.to_string trait_name)))
    end
  | Function (parent_env, _) | Block (parent_env, _) -> lookup_trait parent_env trait_name

let rec lookup_method env method_name =
  match env with
  | Global (_, _, method_map, _, _) -> 
    begin
      match MethodNameMap.find method_map method_name with
      | Some method_defn -> Ok method_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Method %s not found" (Method_name.to_string method_name)))
    end
  | Function (parent_env, _) | Block (parent_env, _) -> lookup_method parent_env method_name

let rec lookup_function env function_name =
  match env with
  | Global (_, _, _, function_map, _) -> 
    begin
      match FunctionNameMap.find function_map function_name with
      | Some function_defn -> Ok function_defn
      | None -> Error (Core.Error.of_string (Fmt.str "Function %s not found" (Function_name.to_string function_name)))
    end
  | Function (parent_env, _) | Block (parent_env, _) -> lookup_function parent_env function_name

let rec lookup_impl env struct_name =
  match env with
  | Global (_, _, _, _, struct_trait_map) ->
    begin 
    match StructTraitMap.find struct_trait_map struct_name with 
    | Some trait_names -> Ok trait_names
    | None -> Error (Core.Error.of_string "Impl lookup should be done in the global environment")
    end 
  | Function (parent_env, _) | Block (parent_env, _) -> lookup_impl parent_env struct_name

let rec lookup_var env var_name =
  match env with
  | Global _ -> Error (Core.Error.of_string (Fmt.str "Variable %s not found" (Var_name.to_string var_name)))
  | Function (parent_env, var_map) | Block (parent_env, var_map) ->
    begin 
      match VarNameMap.find var_map var_name with
      | Some var_type -> Ok var_type
      | None -> lookup_var parent_env var_name
    end

let lookup_method_signature trait_defn method_name = 
  match trait_defn with
  | Ast.TTrait (_, method_signatures) ->
    begin
      match List.find method_signatures ~f:(fun (TMethodSignature (name, _, _, _, _)) -> Method_name.(=) name method_name) with
      | Some method_signature -> Ok method_signature
      | None -> Error (Core.Error.of_string (Fmt.str "Method %s not found" (Method_name.to_string method_name)))
    end

let lookup_method_in_impl env struct_name method_name =
  (* let%bind trait_names = lookup_impl env struct_name in *)
  let glob_env = find_global env in
  match glob_env with
  | Global (_, _, method_map, _, _) ->
    let method_defn_option = MethodNameMap.find method_map method_name in
    begin match method_defn_option with
    | Some method_defn -> Ok method_defn
    | None -> Error (Core.Error.of_string (Fmt.str "Method %s not found in impl for struct %s" (Method_name.to_string method_name) (Struct_name.to_string struct_name)))
    end
  | _ -> Error (Core.Error.of_string "Method lookup in impl should be done in the global environment")

let get_method_map method_defns =
  List.fold method_defns ~init:MethodNameMap.empty ~f:(fun map method_defn ->
    match method_defn with
    | Ast.TMethod (TMethodSignature (method_name, _, _, _, _), _) ->
      MethodNameMap.add_exn map ~key:method_name ~data:method_defn
  )
  
let get_struct_trait_map env =
  match env with
  | Global (_, _, _, _, struct_trait_map) -> struct_trait_map
  | _ -> failwith "get_struct_trait_map should only be called on a global environment"
  
let update_env_with_maps env ~method_map ~struct_trait_map =
  match env with
  | Global (struct_map, trait_map, _, function_map, _) ->
    Global (struct_map, trait_map, method_map, function_map, struct_trait_map)
  | _ -> failwith "update_env_with_maps should only be called on a global environment"
   

(* Add Functions *)
let add_struct_to_global env struct_defn =
  match env, struct_defn with
  | Global (struct_map, trait_map, method_map, function_map, structtrait_map), Ast.TStruct (name, _, _) ->
      let new_struct_map = StructNameMap.add_exn struct_map ~key:name ~data:struct_defn in
      Global (new_struct_map, trait_map, method_map, function_map, structtrait_map)
  | _ -> env

let add_trait_to_global env trait_defn =
  match env, trait_defn with
  | Global (struct_map, trait_map, method_map, function_map, structtrait_map), Ast.TTrait (name, _) ->
      let new_trait_map = TraitNameMap.add_exn trait_map ~key:name ~data:trait_defn in
      Global (struct_map, new_trait_map, method_map, function_map, structtrait_map)
  | _ -> env

let add_method_to_global env method_defn =
  match env, method_defn with
  | Global (struct_map, trait_map, method_map, function_map, structtrait_map), Ast.TMethod (TMethodSignature (method_name, _, _, _, _), _) ->
      let new_method_map = MethodNameMap.add_exn method_map ~key:method_name ~data:method_defn in
      Global (struct_map, trait_map, new_method_map, function_map, structtrait_map)
  | _ -> env

let add_function_to_global env function_defn =
  match env, function_defn with
  | Global (struct_map, trait_map, method_map, function_map, structtrait_map), Ast.TFunction (function_name, _, _, _, _) ->
      let new_function_map = FunctionNameMap.add_exn function_map ~key:function_name ~data:function_defn in
      Global (struct_map, trait_map, method_map, new_function_map, structtrait_map)
  | _ -> env

  let add_impl_to_global env (Ast.TImpl (trait_name, struct_name, method_defns)) =
    let method_map = get_method_map method_defns in
    let global_env = find_global env in
    let struct_trait_map = get_struct_trait_map global_env in
    let existing_traits = match StructTraitMap.find struct_trait_map struct_name with
      | Some traits -> traits
      | None -> []
    in
    let updated_traits = trait_name :: existing_traits in
    let updated_struct_trait_map = StructTraitMap.add_exn struct_trait_map ~key:struct_name ~data:updated_traits in
    update_env_with_maps global_env ~method_map ~struct_trait_map:updated_struct_trait_map
  
let add_function_scope env params =
  Function (env, params)

let add_block_scope env var_map = Block (find_global env, var_map)

let add_var_to_block_scope env var_name var_type =
  match env with
  | Block (parent, params) ->
    let new_params = VarNameMap.add_exn params ~key:var_name ~data:var_type in
    Block (parent, new_params)
  | _ -> env

let add_this_to_block_scope env struct_name =
  let this_type = TEStruct struct_name in
  add_var_to_block_scope env (Var_name.of_string "this") this_type

(* Remove Functions *)
let remove_scope = function
| Global _ -> Error (Base.Error.of_string "Cannot remove global scope")
| Function (parent, _) -> Ok parent
| Block (parent, _) -> Ok parent

(* Getter Functions *)
let get_obj_struct_defn var_name env loc = 
  lookup_var env var_name
  >>= function
  | TEStruct (struct_name) ->
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
    match List.find ~f:(fun (TField (_, _, name, _)) -> Field_name.(=) name field_name) fields with
    | Some (TField(modifier, _, _, _)) ->
      if phys_equal modifier (MConst) then
        Error (Core.Error.of_string 
                (Fmt.str "%d:%d Type error - Cannot assign to const field %s" (loc.lnum) (loc.cnum) (Field_name.to_string field_name)))
      else Ok ()
    | None -> Error (Core.Error.of_string 
                (Fmt.str "%d:%d Type error - Field %s not found in struct" (loc.lnum) (loc.cnum) (Field_name.to_string field_name)))