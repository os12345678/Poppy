open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env
(* open Poppy_parser.Ast_types *)

let capability_names_from_capabilities caps =
  List.map ~f:extract_capability_name caps


let aggregate_capability_accesses_thread_free_var all_vars_capability_accesses
    (obj_name, obj_class, _) =
  List.filter_map
    ~f:(fun (name, class_name, var_capabilities_accessed) ->
      if phys_equal obj_name name && phys_equal class_name obj_class then Some var_capabilities_accessed
      else None)
    all_vars_capability_accesses
  |> fun updated_capabilities_accessed ->
  List.dedup_and_sort
    ~compare:(fun a b -> if phys_equal a b then 0 else 1)
    (List.concat updated_capabilities_accessed)
  |> fun deduped_updated_capabilities_accessed ->
  (obj_name, obj_class, deduped_updated_capabilities_accessed)

  let get_arg_capabilities_used_by_fn env param arg =
    match
      (param_to_obj_var_and_capabilities env param, reduce_expr_to_obj_ids arg)
    with
    | Some (_, _, arg_capabilities_used), possible_expr_reduced_ids ->
        List.filter_map
          ~f:(function
            | TVariable (var_name, var_type, _, _) -> (
              match var_type with
              | TEStruct obj_struct -> Some (var_name, obj_struct, arg_capabilities_used)
              | _                      -> None )
            | TObjField _ ->
                (* If passing in a field of an object, the capabilities required are that of
                   the fields, not the object itself so we don't track. *)
                None)
          possible_expr_reduced_ids
    | None, _ -> []

let use_all_identifier_capabilities id =
  match id with
  | TVariable (var_name, var_type, capabilities, _) -> (
    match var_type with
    | TEStruct obj_struct -> [(var_name, obj_struct, capabilities)]
    | _                      -> [] )
  | TObjField (obj_struct, obj_name, _, _, capabilities, _) ->
      [(obj_name, obj_struct, capabilities)]
    

let choose_identifier_capabilities id =
  (* prefer capabilities with non-linear modes, as least restrictive *)
  match id with
  | TVariable _ ->
      (* just referencing a variable doesn't require us to use capabilities - it's only if
          we access a field or call a method. *)
      []
  | TObjField (obj_struct, obj_name, _, _, capabilities, _) ->
      ( match
          List.find ~f:(fun (TCapability (mode, _)) -> not (phys_equal mode Linear)) capabilities
        with
      | Some capability -> [capability]
      | None            ->
          if
            List.is_empty capabilities
            (* Don't worry if no capability found - this will be caught in later
                type-checking. *)
          then []
          else [List.hd_exn capabilities] )
      |> fun chosen_capabilities -> [(obj_name, obj_struct, chosen_capabilities)]
      
let rec aggregate_capability_accesses_expr (struct_defns: struct_defn list) (method_defns: method_defn list) (impl_defns: impl_defn list) (function_defns: function_defn list) (env: E.env) expr =
  let aggregate_capability_accesses_expr_rec =
    aggregate_capability_accesses_expr struct_defns method_defns impl_defns function_defns env in
  let aggregate_capability_accesses_block_expr_rec =
    aggregate_capability_accesses_block_expr struct_defns method_defns impl_defns function_defns env in
  match expr.node with
  | TInt _ | TBoolean _ -> (expr, [])
  | TIdentifier id -> 
    choose_identifier_capabilities id 
    |> fun capability_accesses ->
    ({ expr with node = TIdentifier id }, capability_accesses)
  | TBlockExpr block_expr ->
    aggregate_capability_accesses_block_expr_rec block_expr
    |> fun (updated_block, capability_accesses) ->
    ({expr with node = TBlockExpr updated_block}, capability_accesses)
  | TConstructor (var_name, struct_name, constructor_args) ->
    List.unzip
      (List.map
          ~f:(fun (ConstructorArg (field_name, expr)) ->
            aggregate_capability_accesses_expr_rec expr
            |> fun (updated_expr, arg_capability_accesses) ->
            ( ConstructorArg (field_name, updated_expr)
            , arg_capability_accesses ))
          constructor_args)
    |> fun (updated_args, args_capability_accesses) ->
    ({expr with node = TConstructor (var_name, struct_name, updated_args)}
    , List.concat args_capability_accesses )
  | TLet (type_expr, var_name, bound_expr) ->
    aggregate_capability_accesses_expr_rec bound_expr
    |> fun (updated_bound_expr, capability_accesses) ->
    ({expr with node = TLet (type_expr, var_name, updated_bound_expr)}, capability_accesses)
  | TAssign (id, assigned_expr) ->
    choose_identifier_capabilities id
    |> fun id_capability_accesses ->
    aggregate_capability_accesses_expr_rec assigned_expr
    |> fun (updated_assigned_expr, expr_capability_accesses) ->
    ({expr with node =  TAssign (id, updated_assigned_expr)}
    , id_capability_accesses @ expr_capability_accesses )
  | TConsume id ->
    use_all_identifier_capabilities id
    |> fun id_capability_accesses -> ({expr with node = TConsume id}, id_capability_accesses)
  | TMethodApp (obj_name, struct_name, trait_name, method_name, obj_method_capabilities, args) ->
    let method_params_result = get_method_params method_name method_defns in
    begin match method_params_result with
    | method_params ->
        List.unzip
          (List.map
              ~f:(fun (param, arg) ->
                aggregate_capability_accesses_expr_rec arg
                |> fun (updated_arg, arg_capability_accesses) ->
                ( updated_arg
                , get_arg_capabilities_used_by_fn env param arg
                  @ arg_capability_accesses ))
              (List.zip_exn method_params args))
        |> fun (updated_args, args_capability_accesses) ->
          get_method_capabilities2 method_name struct_name trait_name impl_defns struct_defns
        |> fun obj_method_capabilities_used ->
        ({expr with node = TMethodApp
            ( obj_name
            , struct_name
            , trait_name
            , method_name
            , obj_method_capabilities
            , updated_args )}
        , (obj_name, struct_name, obj_method_capabilities_used)
          :: List.concat args_capability_accesses )
    end

  | TFunctionApp (func_name, args) ->
    List.unzip
      (List.map
          ~f:(fun (param, arg) ->
            aggregate_capability_accesses_expr_rec arg
            |> fun (updated_arg, arg_capability_accesses) ->
            ( updated_arg
            , get_arg_capabilities_used_by_fn env param arg
              @ arg_capability_accesses ))
          (List.zip_exn (get_function_params func_name function_defns) args))
    |> fun (updated_args, args_capability_accesses) ->
    ({expr with node = TFunctionApp (func_name, updated_args)}
    , List.concat args_capability_accesses )

    | TPrintf (format_str, args) ->
      List.unzip
        (List.map
           ~f:(fun arg ->
             aggregate_capability_accesses_expr_rec arg
             |> fun (updated_arg, arg_capability_accesses) ->
             (updated_arg, arg_capability_accesses))
           args)
      |> fun (updated_args, args_capability_accesses) ->
      ({expr with node = TPrintf (format_str, updated_args)}, List.concat args_capability_accesses)

  | TFinishAsync (async_exprs, curr_thread_free_vars, curr_thread_expr) ->
    List.unzip
      (List.map
          ~f:(fun (AsyncExpr (free_vars, expr)) ->
            aggregate_capability_accesses_block_expr_rec expr
            |> fun (updated_expr, expr_capability_accesses) ->
            List.map
              ~f:(aggregate_capability_accesses_thread_free_var expr_capability_accesses)
              free_vars
            |> fun updated_free_vars ->
            (AsyncExpr (updated_free_vars, updated_expr), expr_capability_accesses))
          async_exprs)
    |> fun (updated_async_exprs, async_exprs_capability_accesses) ->
    aggregate_capability_accesses_block_expr_rec curr_thread_expr
    |> fun (updated_curr_thread_expr, curr_thread_capability_accesses) ->
    List.map
      ~f:(aggregate_capability_accesses_thread_free_var curr_thread_capability_accesses)
      curr_thread_free_vars
    |> fun updated_curr_thread_free_vars ->
    ({expr with node = TFinishAsync
        ( updated_async_exprs
        , updated_curr_thread_free_vars
        , updated_curr_thread_expr )}
    , curr_thread_capability_accesses @ List.concat async_exprs_capability_accesses )
  | TIf (cond_expr, then_expr, else_expr) ->
    aggregate_capability_accesses_expr_rec cond_expr
    |> fun (updated_cond_expr, cond_capability_accesses) ->
    aggregate_capability_accesses_block_expr_rec then_expr
    |> fun (updated_then_expr, then_capability_accesses) ->
    aggregate_capability_accesses_block_expr_rec else_expr
    |> fun (updated_else_expr, else_capability_accesses) ->
    ({expr with node = TIf (updated_cond_expr, updated_then_expr, updated_else_expr)}
    , cond_capability_accesses @ then_capability_accesses @ else_capability_accesses )
  | TWhile (cond_expr, loop_expr) ->
    aggregate_capability_accesses_expr_rec cond_expr
    |> fun (updated_cond_expr, cond_capability_accesses) ->
    aggregate_capability_accesses_block_expr_rec loop_expr
    |> fun (updated_loop_expr, loop_capability_accesses) ->
    ({expr with node = TWhile (updated_cond_expr, updated_loop_expr)}
    , cond_capability_accesses @ loop_capability_accesses )
  | TBinOp (binop, expr1, expr2) ->
    aggregate_capability_accesses_expr_rec expr1
    |> fun (updated_expr1, expr1_capability_accesses) ->
    aggregate_capability_accesses_expr_rec expr2
    |> fun (updated_expr2, expr2_capability_accesses) ->
    ({expr with node = TBinOp (binop, updated_expr1, updated_expr2)}
    , expr1_capability_accesses @ expr2_capability_accesses )
  | TUnOp (unop, expr) ->
    aggregate_capability_accesses_expr_rec expr
    |> fun (updated_expr, expr_capability_accesses) ->
    ({expr with node = TUnOp (unop, updated_expr)}, expr_capability_accesses)

and aggregate_capability_accesses_block_expr struct_defns method_defns impl_defns function_defns (env: E.env)
  (Block (loc, type_block_expr, exprs)) =
  List.unzip
    (List.map ~f:(aggregate_capability_accesses_expr struct_defns method_defns impl_defns function_defns env) exprs)
  |> fun (updated_exprs, capability_accesses) ->
  (Block (loc, type_block_expr, updated_exprs), List.concat capability_accesses)