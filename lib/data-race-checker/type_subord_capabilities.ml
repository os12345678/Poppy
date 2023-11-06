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
  | TConstructor (var_name, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (field_name, expr)) ->
            ConstructorArg
              (field_name, remove_subord_capabilities_expr env expr))
          constructor_args in
      {expr with node = TConstructor (var_name, class_name, updated_args)}
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
let type_subord_capabilities_block_expr class_defns obj_vars_and_capabilities block_expr =
  if is_this_present obj_vars_and_capabilities then block_expr
  else remove_subord_capabilities_block_expr class_defns block_expr
    