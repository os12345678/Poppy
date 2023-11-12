open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env

let remove_subord_capabilities env struct_name capabilities =
  List.filter
    ~f:(fun capability ->
      not (capability_fields_have_mode capability struct_name Subordinate env))
    capabilities

let remove_subord_capabilities_id env id =
  match id with
  | TVariable (var_name, var_type, caps, maybeBorrowed) -> (
    match var_type with
    | TEStruct var_struct ->
        TVariable
          ( var_name
          , var_type
          , remove_subord_capabilities env var_struct caps
          , maybeBorrowed )
    | _                      -> id (* nothing to update *) )
  | TObjField (obj_struct, obj_name, field_type, field_name, caps, maybeBorrowed) ->
      TObjField
        ( obj_struct
        , obj_name
        , field_type
        , field_name
        , remove_subord_capabilities env obj_struct caps
        , maybeBorrowed )

let rec remove_subord_capabilities_expr env expr =
  match expr.node with
  | TInt _ | TBoolean _ -> expr
  | TIdentifier id -> {expr with node = TIdentifier (remove_subord_capabilities_id env id)}
  | TBlockExpr block_expr -> {expr with node = 
    TBlockExpr (remove_subord_capabilities_block_expr env block_expr)}
  | TConstructor (var_name, struct_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (field_name, expr)) ->
            ConstructorArg
              (field_name, remove_subord_capabilities_expr env expr))
          constructor_args in
      {expr with node = TConstructor (var_name, struct_name, updated_args)}
  | TLet (type_expr, var_name, bound_expr) ->
    {expr with node = TLet
      (type_expr, var_name, remove_subord_capabilities_expr env bound_expr)}
  | TAssign (id, assigned_expr) ->
      {expr with node = TAssign
        ( remove_subord_capabilities_id env id
        , remove_subord_capabilities_expr env assigned_expr )}
  | TConsume id -> {expr with node = TConsume (remove_subord_capabilities_id env id)}
  | TMethodApp (obj_name, obj_struct, obj_trait, method_name, obj_capabilities, args)
  -> {expr with node = 
    TMethodApp
      ( obj_name
      , obj_struct
      , obj_trait
      , method_name
      , remove_subord_capabilities env obj_struct obj_capabilities
      , List.map ~f:(remove_subord_capabilities_expr env) args )}
  | TFunctionApp (func_name, args) -> {expr with node = 
    TFunctionApp
      ( func_name
      , List.map ~f:(remove_subord_capabilities_expr env) args )}
  | TPrintf (format_str, args) -> {expr with node = 
    TPrintf
      (format_str, List.map ~f:(remove_subord_capabilities_expr env) args)}
  | TFinishAsync (async_exprs, curr_thread_free_vars, curr_thread_expr) ->
    {expr with node = TFinishAsync
      ( List.map
          ~f:(fun (AsyncExpr (free_vars, expr)) ->
            AsyncExpr (free_vars, remove_subord_capabilities_block_expr env expr))
          async_exprs
      , curr_thread_free_vars
      , remove_subord_capabilities_block_expr env curr_thread_expr )}
  | TIf (cond_expr, then_expr, else_expr) ->
    {expr with node = TIf
      ( remove_subord_capabilities_expr env cond_expr
      , remove_subord_capabilities_block_expr env then_expr
      , remove_subord_capabilities_block_expr env else_expr )}
  | TWhile (cond_expr, loop_expr) ->
    {expr with node = TWhile
      ( remove_subord_capabilities_expr env cond_expr
      , remove_subord_capabilities_block_expr env loop_expr )}
  | TBinOp (binop, expr1, expr2) ->
    {expr with node = TBinOp
      ( binop
      , remove_subord_capabilities_expr env expr1
      , remove_subord_capabilities_expr env expr2 )}
  | TUnOp (unop, expr) ->
      {expr with node = TUnOp (unop, remove_subord_capabilities_expr env expr)}

and remove_subord_capabilities_block_expr env
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs = List.map ~f:(remove_subord_capabilities_expr env) exprs in
  Block (loc, type_block_expr, updated_exprs)

let is_this_present obj_vars_and_capabilities =
  let obj_var_names, _, _ = List.unzip3 obj_vars_and_capabilities in
  elem_in_list (Var_name.of_string "this") obj_var_names
  

(* If we are not in an object method then we can't access subordinate state, so remove
   access to subordinate state. We do this by checking if "this" is a param of the method. *)
let type_subord_capabilities_block_expr struct_defns obj_vars_and_capabilities block_expr =
  if is_this_present obj_vars_and_capabilities then block_expr
  else remove_subord_capabilities_block_expr struct_defns block_expr
    
  let type_subord_capabilities_method_prototype struct_defns obj_struct meth_name ret_type
  param_obj_var_capabilities =
let error_prefix =
  Fmt.str "Potential Data Race in %s's method %s:"
    (Struct_name.to_string obj_struct)
    (Method_name.to_string meth_name) in
(* if object is encapsulated - then it's fine to pass subord state in or out of objects
   as all accesses will be via object's owner. *)
if struct_has_mode obj_struct Encapsulated struct_defns then Ok ()
else if type_has_mode ret_type Subordinate struct_defns then
  Error
    (Error.of_string
       (Fmt.str "%s Subordinate state returned by non-encapsulated method" error_prefix))
else
  let subord_args =
    List.filter
      ~f:(fun (_, var_struct, capabilities) ->
        List.exists
          ~f:(fun capability ->
            capability_fields_have_mode capability var_struct Subordinate struct_defns)
          capabilities)
      param_obj_var_capabilities in
  if List.is_empty subord_args then Ok ()
  else
    let subord_arg_str =
      String.concat ~sep:", "
        (List.map ~f:(fun (var_name, _, _) -> Var_name.to_string var_name) subord_args)
    in
    Error
      (Error.of_string
         (Fmt.str "%s Subordinate arguments passed into non-encapsulated method: %s"
            error_prefix subord_arg_str))