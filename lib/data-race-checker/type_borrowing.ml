open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env

let check_arg_borrowing env loc ((Param (param_type, _, _, maybe_borrowed) as param), arg_expr) = 
  match (param_type, maybe_borrowed) with
  | TEStruct sname, None -> 
    let _, _, param_capabilities = 
    List.unzip3 (params_to_obj_vars_and_capabilities env [param]) in
    let is_param_linear = 
      List.exists ~f:(fun capability -> 
        capability_fields_have_mode capability sname Linear env) (List.concat param_capabilities) in
    if is_param_linear then
      if List.is_empty (reduce_expr_to_obj_ids arg_expr) then Ok ()
      else Error (Error.of_string (Fmt.str "%s Linear non-borrowed argument should be consumed@." (string_of_loc loc)))
    else Ok ()
  | TEStruct _, Some Borrowed | _ -> Ok ()

  let rec type_function_forward_borrowing_expr method_defns function_defns env expr =
    let open Result in
    match expr.node with
    | TMethodApp (obj_name, obj_struct, _, meth_name, _, args) ->
      let args_ids = List.concat_map ~f:reduce_expr_to_obj_ids args in
      Type_linear_capabilities.type_linear_obj_method_args env obj_name obj_struct args_ids expr.loc
      >>= fun () ->
      Type_linear_capabilities.type_linear_args env args_ids expr.loc
      >>= fun () ->
      (match get_method_params meth_name method_defns with
       | params ->
          Result.all_unit
            (List.map ~f:(check_arg_borrowing env expr.loc) (List.zip_exn params args))
          >>= fun () ->
          (* Recurse on arguments *)
          Result.all_unit
            (List.map
               ~f:(type_function_forward_borrowing_expr method_defns function_defns env)
               args))

    | TFunctionApp (func_name, args) ->
    let args_ids = List.concat_map ~f:reduce_expr_to_obj_ids args in
    Type_linear_capabilities.type_linear_args env args_ids expr.loc
    >>= fun () ->
      (match get_function_params func_name function_defns with
        | params ->
          Result.all_unit
            (List.map ~f:(check_arg_borrowing env expr.loc) (List.zip_exn params args))
              >>= fun () ->
              (* Recurse on arguments *)
              Result.all_unit
                (List.map
                    ~f:(type_function_forward_borrowing_expr method_defns function_defns env)
                    args)) 
    | T.TInt _ | T.TBoolean _ -> Ok ()
    | T.TIdentifier _ -> Ok ()
    | T.TBlockExpr block_expr ->
        (type_function_forward_borrowing_block_expr method_defns function_defns env) block_expr
    | T.TConstructor (_, _, constructor_args) ->
        Result.all_unit
          (List.map
              ~f:(fun (ConstructorArg (_, expr)) ->
                type_function_forward_borrowing_expr method_defns function_defns env expr)
              constructor_args)
    | T.TLet (_, _, bound_expr) ->
        type_function_forward_borrowing_expr method_defns function_defns env bound_expr
    | T.TAssign (_, assigned_expr) ->
        type_function_forward_borrowing_expr method_defns function_defns env assigned_expr
    | T.TConsume _ -> Ok ()
    | T.TPrintf (_, args) ->
        Result.all_unit
          (List.map
              ~f:(type_function_forward_borrowing_expr method_defns function_defns env)
              args)
    | T.TFinishAsync (async_exprs, _, curr_thread_expr) ->
        Result.all_unit
          (List.map
              ~f:(fun (AsyncExpr (_, expr)) ->
                (type_function_forward_borrowing_block_expr method_defns function_defns env) expr)
              async_exprs)
        >>= fun () ->
        (type_function_forward_borrowing_block_expr method_defns function_defns env)
          curr_thread_expr
    | T.TIf (cond_expr, then_expr, else_expr) ->
        type_function_forward_borrowing_expr method_defns function_defns env cond_expr
        >>= fun () ->
        (type_function_forward_borrowing_block_expr method_defns function_defns env) then_expr
        >>= fun () ->
        (type_function_forward_borrowing_block_expr method_defns function_defns env) else_expr
    | T.TWhile (cond_expr, loop_expr) ->
        type_function_forward_borrowing_expr method_defns function_defns env cond_expr
        >>= fun () ->
        (type_function_forward_borrowing_block_expr method_defns function_defns env) loop_expr
    | T.TBinOp (_, expr1, expr2) ->
        type_function_forward_borrowing_expr method_defns function_defns env expr1
        >>= fun () -> type_function_forward_borrowing_expr method_defns function_defns env expr2
    | T.TUnOp (_, expr) ->
        type_function_forward_borrowing_expr method_defns function_defns env expr
  
  and type_function_forward_borrowing_block_expr method_defns function_defns env (Block (_, _, exprs)) =
    Result.all_unit
      (List.map ~f:(type_function_forward_borrowing_expr method_defns function_defns env) exprs)
  

let id_maybe_borrowed = function
  | TVariable (_, _, _, maybe_borrowed) -> maybe_borrowed
  | TObjField (_, _, _, _, _, maybe_borrowed) -> maybe_borrowed

let id_is_borrowed id =
  match id_maybe_borrowed id with Some Borrowed -> true | None -> false

let type_function_reverse_borrowing env error_prefix return_type
    maybe_borrowed_ref_ret body_expr =
  match return_type with
  | TEStruct struct_name ->
      if struct_has_mode struct_name Linear env then
        match reduce_block_expr_to_obj_ids body_expr with
        | []  -> Ok ()
        | ids -> (
            if List.exists ~f:id_is_borrowed ids then
              Error
                (Error.of_string
                   (Fmt.str
                      "%s Body expression may return a borrowed type, which is not allowed.@."
                      error_prefix))
            else
              match maybe_borrowed_ref_ret with
              | Some Borrowed -> Ok () (* can reverse borrow ref*)
              | None          ->
                  Error
                    (Error.of_string
                       (Fmt.str
                          "%s Body expression may return a non-consumed id, which is not allowed as function doesn't borrow result.@."
                          error_prefix)) )
      else Ok () (* if not linear we are not worried about borrowing *)
  | _                       ->
      (* we don't check borrowing for primitive return type *)
      Ok ()

let rec type_assign_borrowed_expr method_defns function_defns env expr =
  let open Result in
  match expr.node with
  | T.TAssign (_, assigned_expr) -> (
      type_assign_borrowed_expr method_defns function_defns env assigned_expr
      >>= function
      | Some Borrowed ->
          Error
            (Core.Error.of_string
                (Fmt.str "%s Type error: Trying to assign a borrowed value@."
                  (string_of_loc expr.loc)))
      | None          -> Ok None )

  | T.TMethodApp ( _, _, _, meth_name, _, args) ->
    (* Recurse on arguments *)
    Result.all (List.map ~f:(type_assign_borrowed_expr method_defns function_defns env) args)
    >>| fun _ ->
    get_method_defn meth_name method_defns
    |> fun (TMethod (method_signature, _)) -> method_signature.borrowed

  | T.TFunctionApp (func_name, args) ->
    (* Recurse on arguments *)
    Result.all (List.map ~f:(type_assign_borrowed_expr method_defns function_defns env) args)
    >>| fun _ ->
    get_function_defn func_name function_defns
    |> fun (TFunction (function_signature, _)) -> function_signature.borrowed

  | T.TInt _ | T.TBoolean _ -> Ok None
  | T.TIdentifier id -> Ok (id_maybe_borrowed id)
  | T.TBlockExpr block_expr ->
      (type_assign_borrowed_block_expr method_defns function_defns env) block_expr
  | T.TConstructor (_, _, constructor_args) ->
      Result.all
        (List.map
            ~f:(fun (ConstructorArg (_, expr)) ->
              type_assign_borrowed_expr method_defns function_defns env expr)
            constructor_args)
      >>| fun _ -> None
  | T.TLet (_, _, bound_expr) ->
      type_assign_borrowed_expr method_defns function_defns env bound_expr
  | T.TConsume id -> Ok (id_maybe_borrowed id)
  | T.TPrintf (_, args) ->
      Result.all (List.map ~f:(type_assign_borrowed_expr method_defns function_defns env) args)
      >>| fun _ -> None
  | T.TFinishAsync (async_exprs, _, curr_thread_expr) ->
      Result.all
        (List.map
            ~f:(fun (AsyncExpr (_, expr)) ->
              (type_assign_borrowed_block_expr method_defns function_defns env) expr)
            async_exprs)
      >>= fun _ ->
      (type_assign_borrowed_block_expr method_defns function_defns env) curr_thread_expr
  | T.TIf (cond_expr, then_expr, else_expr) -> (
    type_assign_borrowed_expr method_defns function_defns env cond_expr
    >>= fun _ ->
    (type_assign_borrowed_block_expr method_defns function_defns env) then_expr
    >>= fun then_maybe_borrowed ->
    (type_assign_borrowed_block_expr method_defns function_defns env) else_expr
    >>| fun else_maybe_borrowed ->
    match (then_maybe_borrowed, else_maybe_borrowed) with
    | None, None -> None
    | _          -> Some Borrowed )
  | T.TWhile (cond_expr, loop_expr) ->
      type_assign_borrowed_expr method_defns function_defns env cond_expr
      >>= fun _ ->
      (type_assign_borrowed_block_expr method_defns function_defns env) loop_expr
      >>| fun _ -> None
  (* loop returns void *)
  | T.TBinOp ( _, expr1, expr2) ->
      type_assign_borrowed_expr method_defns function_defns env expr1
      >>= fun _ ->
      type_assign_borrowed_expr method_defns function_defns env expr2 >>| fun _ -> None
  (* we don't return borrowed ref as we don't return object *)
  | T.TUnOp (_, expr) ->
      type_assign_borrowed_expr method_defns function_defns env expr >>| fun _ -> None

  and type_assign_borrowed_block_expr method_defns function_defns env (T.Block (_, _, exprs)) =
    let open Result in
    Result.all (List.map ~f:(type_assign_borrowed_expr method_defns function_defns env) exprs)
    >>| function [] -> None | list -> List.last_exn list