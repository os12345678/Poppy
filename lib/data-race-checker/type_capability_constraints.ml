open Core 
open Data_race_env
open Update_identifier_capabilities
open Poppy_type_checker.Typed_ast
open Poppy_parser.Ast_types
open Type_consume
open Type_concurrent_capability_access


let type_param_capability_constraints param_names_and_capabilities block_expr =
  List.fold ~init:block_expr
    ~f:(fun acc_expr (param_name, _, param_allowed_caps) ->
      (* for each param, get it and all aliases and filter their capabilities to only the
         allowed capabilities *)
      let param_aliases =
        find_aliases_in_block_expr ~should_match_fields:false param_name block_expr in
      update_matching_identifier_caps_block_expr (param_name :: param_aliases)
        (fun _ cap -> elem_in_list cap param_allowed_caps)
        acc_expr)
    param_names_and_capabilities

let type_obj_method_capability_constraints method_defns obj_name method_name
    obj_capabilities loc =
  let required_capabilities =
    get_method_capabilities_used method_name method_defns in
  if is_subset_of required_capabilities obj_capabilities then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str
            "%s Potential data race: %s's method %s's capability constraints not satisfied."
            (string_of_loc loc) (Var_name.to_string obj_name)
            (Method_name.to_string method_name)))

(* since tracking aliasing once an expression is assigned to a field of an object is
  intractable, we require that if we assign an expression to a field, that all
  capabilities are available to the field being assigned to. *)
  let type_capability_constraints_assigned_expr struct_defns type_expr assign_expr loc =
  let assign_expr_reduced_ids = reduce_expr_to_obj_ids assign_expr in
  let ids_satisfy_capability_constraints =
    List.for_all
      ~f:(fun reduced_id ->
        match (type_expr, reduced_id) with
        | TEStruct struct_name, TVariable (_, _, var_capabilities, _) ->
            let required_capabilities = get_struct_capabilities struct_name struct_defns in
            is_subset_of required_capabilities var_capabilities
        | _ -> true)
      assign_expr_reduced_ids in
  if ids_satisfy_capability_constraints then Ok ()
  else
    Error
      (Error.of_string
          (Fmt.str "%s Assigned expression doesn't have all capabilities available@."
            (string_of_loc loc)))

let type_capability_constraints_function_arg env function_str loc (param, arg) =
  let _, _, param_capabilities =
    List.unzip3 (params_to_obj_vars_and_capabilities env [param]) in
  let possible_reduced_arg_ids = reduce_expr_to_obj_ids arg in
  if
    List.for_all
      ~f:(function
        | TVariable (_, _, var_capabilities, _) ->
            is_subset_of (List.concat param_capabilities) var_capabilities
        | TObjField _ -> true)
      possible_reduced_arg_ids
  then Ok ()
  else
    Error
      (Error.of_string
          (Fmt.str
            "%s Potential data race: %s's argument capability constraints not satisfied."
            (string_of_loc loc) function_str))

let type_capabilities_constraints_identifier id loc =
  match id with
  | TVariable _ -> Ok ()
  (* Holding a reference to an object doesn't require any capabilities - only needed to
      access internal state. *)
  | TObjField (_, _, _, _, capabilities, _) ->
      if List.is_empty capabilities then
        Error
          (Error.of_string
              (Fmt.str "%s Potential data race: no allowed capabilities for %s@."
                (string_of_loc loc) (string_of_id id)))
      else Ok ()

let rec type_capabilities_constraints_expr (struct_defns: struct_defn list) (method_defns: method_defn list) (function_defns: function_defn list) (env: E.env) (expr: expr) =
  let open Result in
  match expr.node with
  | TInt _ | TBoolean _ -> Ok ()
  | TIdentifier id -> type_capabilities_constraints_identifier id expr.loc
  | TBlockExpr block_expr ->
    (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env) block_expr
  | TConstructor (_, constructor_args) ->
      Result.all_unit
        (List.map
          ~f:(fun (ConstructorArg (_, expr)) ->
            (type_capabilities_constraints_expr struct_defns method_defns function_defns env) expr)
          constructor_args)
  | TLet (_, _, bound_expr) ->
    (type_capabilities_constraints_expr struct_defns method_defns function_defns env) bound_expr
  | TAssign (id, assigned_expr) ->
    type_capabilities_constraints_identifier id expr.loc
    >>= fun () ->
    type_capability_constraints_assigned_expr struct_defns expr.typ assigned_expr expr.loc
    >>= fun () ->
    (type_capabilities_constraints_expr struct_defns method_defns function_defns env) assigned_expr
  | TConsume id -> type_capabilities_constraints_identifier id expr.loc
  | TMethodApp (obj_name, _obj_struct, _trait_name, method_name, obj_capabilities, args) ->
    let params = get_method_params method_name method_defns in
    let method_str =
      Fmt.str "Obj %s's method %s" (Var_name.to_string obj_name)
        (Method_name.to_string method_name) in
    Result.all_unit
      (List.map
         ~f:(type_capability_constraints_function_arg struct_defns method_str expr.loc)
         (List.zip_exn params args))
    >>= fun () ->
    type_obj_method_capability_constraints method_defns obj_name method_name
      (capability_names_from_capabilities obj_capabilities) expr.loc
    >>= fun () ->
    Result.all_unit
      (List.map ~f:(type_capabilities_constraints_expr struct_defns method_defns function_defns env) args)
  | TFunctionApp (func_name, args) ->
    let params = get_function_params func_name function_defns in
    let function_str = Fmt.str "Function %s" (Function_name.to_string func_name) in
    Result.all_unit
      (List.map
          ~f:(type_capability_constraints_function_arg struct_defns function_str expr.loc)
          (List.zip_exn params args))
    >>= fun () ->
    Result.all_unit
      (List.map ~f:(type_capabilities_constraints_expr struct_defns method_defns function_defns env) args)
  | TPrintf (_, args) ->
    Result.all_unit
      (List.map ~f:(type_capabilities_constraints_expr struct_defns method_defns function_defns env) args)
  | TFinishAsync (async_exprs, curr_thread_free_vars, curr_thread_expr) ->
    let all_async_free_vars =
      List.map ~f:(fun (AsyncExpr (async_free_vars, _)) -> async_free_vars) async_exprs
    in
    type_concurrent_capability_constraints_vars struct_defns
      (curr_thread_free_vars @ List.concat all_async_free_vars)
      expr.loc
    >>= fun () ->
    Result.all_unit
      (List.map
          ~f:(fun (AsyncExpr (_, expr)) ->
            (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env) expr)
          async_exprs)
    >>= fun () ->
    (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env)
      curr_thread_expr
  | TIf (cond_expr, then_expr, else_expr) ->
    (type_capabilities_constraints_expr struct_defns method_defns function_defns env) cond_expr
    >>= fun () ->
    (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env) then_expr
    >>= fun () ->
    (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env) else_expr
  | TWhile (cond_expr, loop_expr) ->
    (type_capabilities_constraints_expr struct_defns method_defns function_defns env) cond_expr
    >>= fun () ->
    (type_capabilities_constraints_block_expr struct_defns method_defns function_defns env) loop_expr
  | TBinOp (_, expr1, expr2) ->
      (type_capabilities_constraints_expr struct_defns method_defns function_defns env) expr1
      >>= fun () -> (type_capabilities_constraints_expr struct_defns method_defns function_defns env) expr2
  | TUnOp ( _, expr) ->
      (type_capabilities_constraints_expr struct_defns method_defns function_defns env) expr

and type_capabilities_constraints_block_expr (struct_defns: struct_defn list) 
(method_defns: method_defn list) (function_defns: function_defn list) (env: E.env) (Block (_, _, exprs)) =
  Result.all_unit
  (List.map ~f:(type_capabilities_constraints_expr struct_defns method_defns function_defns env) exprs)