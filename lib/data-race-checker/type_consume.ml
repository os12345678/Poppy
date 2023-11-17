open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env
(* open Poppy_type_checker.Type_env *)

(* let unwrap_option = function
  | Some x -> x
  | None -> expr.typ *)


let string_of_id id =
  match id with
  | T.TVariable (var_name, _, _, maybe_borrowed) ->
      Fmt.str "%sVariable: %s"
        (string_of_maybe_borrowed_ref maybe_borrowed)
        (Var_name.to_string var_name)
  | T.TObjField (obj_struct, var_name, _, field_name, _, maybe_borrowed) ->
      Fmt.str "%sObjfield: (Struct: %s) %s.%s"
        (string_of_maybe_borrowed_ref maybe_borrowed)
        (Struct_name.to_string obj_struct)
        (Var_name.to_string var_name)
        (Field_name.to_string field_name)

let check_identifiers_disjoint id affected_id =
  match id with
  | T.TVariable (var_name, _, _, _) -> (
    match affected_id with
    | TVariable _ -> not (phys_equal id affected_id)
    | TObjField (_, obj_name, _, _, _, _) -> not (Var_name.(=) var_name obj_name) )
  | TObjField _                   -> not (phys_equal id affected_id)

let remove_reassigned_id reassigned_id consumed_ids =
  List.filter ~f:(check_identifiers_disjoint reassigned_id) consumed_ids
  
  let check_identifier_accessible id consumed_ids =
    List.filter
      ~f:(fun consumed_id -> not (check_identifiers_disjoint consumed_id id))
      consumed_ids
    |> function
    | [] -> Ok ()
    | _  ->
        Error
          (Error.of_string
             (Fmt.str "Type error: Variable %s accessed after being consumed.@."
                (string_of_id id)))

let is_identifier_linear id env =
  match id with
  | TVariable (_, var_type, capabilities, _) -> (
    match var_type with
    (* Check if variable linear *)
    | TEStruct var_struct ->
        List.exists
          ~f:(fun capability ->
            capability_fields_have_mode capability var_struct Linear env)
          capabilities
    | _ -> false )
  | TObjField (_, _, field_type, _, capabilities, _) ->
      (* check we're accessing a linear field and we have a possible capability through
          which we can access it *)
      let is_field_linear = type_has_mode field_type Linear env in
      is_field_linear && not (List.is_empty capabilities)
                
let check_identifier_consumable env id consumed_ids =
  let open Result in
  check_identifier_accessible id consumed_ids
  >>= fun () ->
  if is_identifier_linear id env then Ok ()
  else
    Error
      (Core.Error.of_string
          (Fmt.str "Type error: Trying to consume %s but it is not linear@."
            (string_of_id id)))


let rec check_shared_var_not_consumed var_name = function
  | [] -> Ok ()
  | TVariable (name, _, _, _) :: ids | TObjField (_, name, _, _, _, _) :: ids ->
    if Var_name.(=) var_name name then
      Error
        (Error.of_string
            (Fmt.str "Type error: shared variable %s was consumed."
              (Var_name.to_string var_name)))
    else check_shared_var_not_consumed var_name ids
      
let rec accumulate_consumed_ids env consumed_ids_acc_res expr =
  let open Result in
  consumed_ids_acc_res
  >>= fun consumed_ids_acc -> type_consume_expr env expr consumed_ids_acc
    
and type_consume_expr env expr consumed_ids =
  let open Result in
  match expr.node with
  | T.TInt _ -> Ok consumed_ids
  | TBoolean _ -> Ok consumed_ids
  | TIdentifier id ->
      check_identifier_accessible id consumed_ids >>| fun () -> consumed_ids
  | TBlockExpr block_expr ->
      type_consume_block_expr env block_expr consumed_ids
  | TConstructor (_, constructor_args) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(fun acc (ConstructorArg (_, expr)) ->
          accumulate_consumed_ids env acc expr)
        constructor_args

  | TLet (_var_type, var_name, bound_expr) ->
    type_consume_expr env bound_expr consumed_ids
    >>| remove_reassigned_id (TVariable (var_name, expr.typ, [], None))

  | TAssign (identifier, assigned_expr) ->
      type_consume_expr env assigned_expr consumed_ids
      >>| remove_reassigned_id identifier
  | TConsume id ->
      check_identifier_consumable env id consumed_ids
      >>| fun () -> id :: consumed_ids
  | TMethodApp (obj_name, _, _, _, _, args_exprs) ->
    List.fold ~init:(Ok consumed_ids)
      ~f:(accumulate_consumed_ids env)
      args_exprs
    >>= fun updated_consumed_ids ->
    (* check the identifiers the args reduce to have not been already consumed *)
    Result.all_unit
      (List.map
          ~f:(fun arg_expr ->
            Result.all_unit
              (List.map
                ~f:(fun id -> check_identifier_accessible id updated_consumed_ids)
                (reduce_expr_to_obj_ids arg_expr)))
          args_exprs)
    >>= fun () ->
    (* Check if object hasn't been consumed - i.e. we can call this method *)
    check_identifier_accessible
      (TVariable (obj_name, expr.typ, [], None))
      updated_consumed_ids
    >>| fun () -> updated_consumed_ids
  | TFunctionApp (_, args_exprs) ->
    List.fold ~init:(Ok consumed_ids)
      ~f:(accumulate_consumed_ids env)
      args_exprs
    >>= fun updated_consumed_ids ->
    (* check the identifiers the args reduce to have not been already consumed *)
    Result.all_unit
      (List.map
          ~f:(fun arg_expr ->
            Result.all_unit
              (List.map
                ~f:(fun id -> check_identifier_accessible id updated_consumed_ids)
                (reduce_expr_to_obj_ids arg_expr)))
          args_exprs)
    >>| fun () -> updated_consumed_ids
  | TPrintf (_, args_exprs) ->
    List.fold ~init:(Ok consumed_ids)
      ~f:(accumulate_consumed_ids env)
      args_exprs
  | TFinishAsync (async_exprs, current_thread_free_vars, curr_thread_expr) ->
    let all_thread_exprs =
      AsyncExpr (current_thread_free_vars, curr_thread_expr) :: async_exprs in
      (* For each thread, check that it doesn't consume any shared variables used by other
        threads *)
    Result.all
      (List.map
          ~f:(fun async_expr ->
            let other_async_exprs =
              List.filter ~f:(fun expr -> not (phys_equal expr async_expr)) all_thread_exprs in
            type_consume_async_expr env async_expr other_async_exprs consumed_ids)
          all_thread_exprs)
    >>= fun _ ->
    (* type check each thread individually and aggregate consumed ids (to return) *)
    Result.all
      (List.map
          ~f:(fun (AsyncExpr (_, expr)) ->
            type_consume_block_expr env expr consumed_ids)
          all_thread_exprs)
    >>| fun thread_consumed_id_lists -> List.concat thread_consumed_id_lists
  | TIf (cond_expr, then_expr, else_expr) ->
    type_consume_expr env cond_expr consumed_ids
    >>= fun consumed_ids_with_cond ->
    type_consume_block_expr env then_expr consumed_ids_with_cond
    >>= fun consumed_ids_then ->
    type_consume_block_expr env else_expr consumed_ids_with_cond
    >>| fun consumed_ids_else -> consumed_ids_then @ consumed_ids_else
  | TWhile (cond_expr, loop_expr) ->
    (* Note we check twice to simulate going through loop multiple times *)
    type_consume_expr env cond_expr consumed_ids
    >>= fun consumed_ids_with_cond1 ->
    type_consume_block_expr env loop_expr consumed_ids_with_cond1
    >>= fun consumed_ids_loop1 ->
    type_consume_expr env cond_expr consumed_ids_loop1
    >>= fun consumed_ids_with_cond2 ->
    type_consume_block_expr env loop_expr consumed_ids_with_cond2
  | TBinOp (_, expr1, expr2) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(accumulate_consumed_ids env)
        [expr1; expr2]
  | TUnOp (_, expr) -> type_consume_expr env expr consumed_ids

and type_consume_block_expr env (Block (_, _, block_exprs)) consumed_ids =
  List.fold ~init:(Ok consumed_ids) ~f:(accumulate_consumed_ids env) block_exprs

and type_consume_async_expr env (AsyncExpr (_, async_expr)) other_async_exprs
  consumed_ids =
  (* Check that any shared variables used by other threads were not consumed by this
    threads *)
  let open Result in
  let shared_variables =
    List.concat_map
      ~f:(fun (AsyncExpr (free_vars_and_types, _)) ->
        List.map ~f:(fun (var_name, _, _) -> var_name) free_vars_and_types)
      other_async_exprs in
  type_consume_block_expr env async_expr consumed_ids
  >>= fun thread_consumed_ids ->
  Result.all_unit
    (List.map
      ~f:(fun shared_var -> check_shared_var_not_consumed shared_var thread_consumed_ids)
      shared_variables)

