open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env

let update_capabilities_if_live_aliases filter_linear_caps_fn live_aliases capabilities =
  (* if we have live aliases, then the object is not linear at this point, so filter out
     linear capabilties. *)
  if not (List.is_empty live_aliases) then
    List.filter ~f:(filter_linear_caps_fn capabilities) capabilities
  else capabilities

let type_alias_liveness_identifier aliased_obj_name possible_aliases filter_linear_caps_fn
    live_aliases id =
  let id_name = get_identifier_name id in
  if Var_name.(=) id_name aliased_obj_name then
    let maybe_updated_capabilities =
      update_capabilities_if_live_aliases filter_linear_caps_fn live_aliases
        (get_identifier_capabilities id) in
    (set_identifier_capabilities id maybe_updated_capabilities, live_aliases)
  else
    ( match
        List.find
          ~f:(fun poss_alias -> identifier_matches_var_name poss_alias id)
          possible_aliases
      with
    | Some alias -> alias :: live_aliases
    | None       -> live_aliases )
    |> fun updated_live_aliases -> (id, updated_live_aliases)


let rec type_alias_liveness_expr aliased_obj_name possible_aliases filter_linear_caps_fn
    live_aliases (expr: expr) =
  let type_alias_liveness_expr_rec =
    type_alias_liveness_expr aliased_obj_name possible_aliases filter_linear_caps_fn in
  let type_alias_liveness_identifier_rec =
    type_alias_liveness_identifier aliased_obj_name possible_aliases filter_linear_caps_fn
  in
  let type_alias_liveness_block_expr_rec =
    type_alias_liveness_block_expr aliased_obj_name possible_aliases filter_linear_caps_fn
  in
  match expr.node with
  | TIdentifier id -> 
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, updated_live_aliases) ->
      (TIdentifier updated_id, updated_live_aliases)
  | TInt _ | TBoolean _ -> (expr.node, live_aliases)
  | TBlockExpr block_expr ->
      type_alias_liveness_block_expr_rec live_aliases block_expr
      |> fun (updated_block_expr, updated_live_aliases) ->
      (TBlockExpr updated_block_expr, updated_live_aliases)
  | TConstructor (struct_name, constructor_args) ->
      (* Note we fold right since we run in reverse program execution order. *)
      List.fold_right ~init:([], live_aliases)
        ~f:
          (fun (ConstructorArg (field_name, expr)) (acc_args, acc_live_aliases) -> 
          type_alias_liveness_expr_rec acc_live_aliases expr
          |> fun (updated_expr_node, updated_acc_live_aliases) ->
            let updated_expr = { loc = expr.loc; typ = expr.typ; node = updated_expr_node } in
          ConstructorArg (field_name, updated_expr)
          |> fun updated_arg -> (updated_arg :: acc_args, updated_acc_live_aliases))
        constructor_args
      |> fun (updated_args, updated_live_aliases) ->
      (TConstructor (struct_name, updated_args), updated_live_aliases)
  | TLet (type_expr, var_name, bound_expr) ->
      (* remove this var from the set of live aliases *)
      type_alias_liveness_expr_rec
        (List.filter ~f:(fun name -> not (Var_name.(=) name var_name)) live_aliases)
        bound_expr
      |> fun (updated_bound_expr_node, updated_live_aliases) ->
        let updated_bound_expr = { loc = expr.loc; typ = expr.typ; node = updated_bound_expr_node } in
      (TLet (type_expr, var_name, updated_bound_expr), updated_live_aliases)
  | TAssign (id, assigned_expr) ->
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, id_updated_live_aliases) ->
      type_alias_liveness_expr_rec id_updated_live_aliases assigned_expr
      |> fun (updated_assigned_expr_node, post_assigned_expr_live_aliases) ->
        let updated_assigned_expr = { loc = expr.loc; typ = expr.typ; node = updated_assigned_expr_node } in
      ( TAssign (updated_id, updated_assigned_expr)
      , post_assigned_expr_live_aliases )
  | TConsume id ->
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, updated_live_aliases) ->
      (TConsume updated_id, updated_live_aliases)
      (* TODO STRUCT NAME NOT PART OF METHODAPP SIGNATURE *)
  | TMethodApp (obj_name, struct_name, trait_name, method_name, obj_capabilities, args)->
      let obj_id =
        TVariable (obj_name, TEStruct(struct_name), obj_capabilities, None)  in
      type_alias_liveness_identifier_rec live_aliases obj_id
      |> fun (updated_id, live_aliases_before_method_call) ->
      List.fold_right
      ~init:([], live_aliases_before_method_call)
      ~f:(fun arg (acc_args, acc_live_aliases) ->
        type_alias_liveness_expr_rec acc_live_aliases arg
        |> fun (updated_arg_node, updated_acc_live_aliases) ->
        let updated_arg = { arg with node = updated_arg_node } in
        (updated_arg :: acc_args, updated_acc_live_aliases))
      args
      |> fun (updated_args, updated_live_aliases) ->
      ( TMethodApp
          ( obj_name
          , struct_name
          , trait_name
          , method_name
          , get_identifier_capabilities updated_id
          , updated_args )
      , updated_live_aliases )
  | TFunctionApp (func_name, args) ->
      List.fold_right ~init:([], live_aliases)
        ~f:(fun arg (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases arg
          |> fun (updated_arg_node, updated_acc_live_aliases) ->
            let updated_arg = { loc = expr.loc; typ = expr.typ; node = updated_arg_node } in
          (updated_arg :: acc_args, updated_acc_live_aliases))
        args
      |> fun (updated_args, updated_live_aliases) ->
      (TFunctionApp (func_name, updated_args), updated_live_aliases)
  | TPrintf (format_str, args) ->
      List.fold_right ~init:([], live_aliases)
        ~f:(fun arg (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases arg
          |> fun (updated_arg_node, updated_acc_live_aliases) ->
            let updated_arg = { loc = expr.loc; typ = expr.typ; node = updated_arg_node } in
          (updated_arg :: acc_args, updated_acc_live_aliases))
        args
      |> fun (updated_args, updated_live_aliases) ->
      (TPrintf (format_str, updated_args), updated_live_aliases)
  | TFinishAsync (async_exprs, free_objs, curr_thread_expr) ->
      (* note the async expressions are forked, so we treat as independent (hence map not
         fold) *)
      List.unzip
        (List.map
           ~f:(fun (AsyncExpr (free_objs, async_expr)) ->
             type_alias_liveness_block_expr_rec live_aliases async_expr
             |> fun (updated_async_expr, updated_async_live_aliases) ->
             (AsyncExpr (free_objs, updated_async_expr), updated_async_live_aliases))
           async_exprs)
      |> fun (updated_async_exprs, async_live_aliases) ->
      type_alias_liveness_block_expr_rec live_aliases curr_thread_expr
      |> fun (updated_curr_thread_expr, curr_thread_live_aliases) ->
      List.concat (curr_thread_live_aliases :: async_live_aliases)
      |> fun updated_live_aliases ->
      ( TFinishAsync
          ( updated_async_exprs
          , free_objs
          , updated_curr_thread_expr )
      , updated_live_aliases )
  | TIf (cond_expr, then_expr, else_expr) ->
      type_alias_liveness_block_expr_rec live_aliases then_expr
      |> fun (updated_then_expr, then_live_aliases) ->
      type_alias_liveness_block_expr_rec live_aliases else_expr
      |> fun (updated_else_expr, else_live_aliases) ->
      type_alias_liveness_expr_rec (then_live_aliases @ else_live_aliases) cond_expr
      |> fun (updated_cond_expr_node, cond_live_aliases) ->
        let updated_cond_expr = { loc = expr.loc; typ = expr.typ; node = updated_cond_expr_node } in
      ( TIf (updated_cond_expr, updated_then_expr, updated_else_expr)
      , cond_live_aliases )
  | TWhile (cond_expr, loop_expr) ->
      type_alias_liveness_loop_expr aliased_obj_name possible_aliases
        filter_linear_caps_fn live_aliases loop_expr
      |> fun (updated_loop_expr, loop_live_aliases) ->
      type_alias_liveness_expr_rec loop_live_aliases cond_expr
      |> fun (updated_cond_expr_node, cond_live_aliases) ->
        let updated_cond_expr = { loc = expr.loc; typ = expr.typ; node = updated_cond_expr_node } in
      (TWhile (updated_cond_expr, updated_loop_expr), cond_live_aliases)
  | TBinOp (binop, expr1, expr2) ->
      (* right-to-left as opposite of program execution order *)
      type_alias_liveness_expr_rec live_aliases expr2
      |> fun (updated_expr2_node, expr2_live_aliases) ->
        let updated_expr2 = { loc = expr.loc; typ = expr.typ; node = updated_expr2_node } in
      type_alias_liveness_expr_rec expr2_live_aliases expr1
      |> fun (updated_expr_1_node, expr1_live_aliases) ->
        let updated_expr_1 = { loc = expr.loc; typ = expr.typ; node = updated_expr_1_node } in
      (TBinOp (binop, updated_expr_1, updated_expr2), expr1_live_aliases)
  | TUnOp (unop, expr) ->
      type_alias_liveness_expr_rec live_aliases expr
      |> fun (updated_expr_node, updated_live_aliases) ->
        let updated_expr = { loc = expr.loc; typ = expr.typ; node = updated_expr_node } in
      (TUnOp (unop, updated_expr), updated_live_aliases)


and type_alias_liveness_block_expr aliased_obj_name possible_aliases filter_linear_caps_fn
    live_aliases (Block (loc, type_expr, exprs)) =
  let type_alias_liveness_expr_rec =
    type_alias_liveness_expr aliased_obj_name possible_aliases filter_linear_caps_fn in
  List.fold_right ~init:([], live_aliases)
    ~f:(fun expr (acc_exprs, acc_live_aliases) ->
      type_alias_liveness_expr_rec acc_live_aliases expr
      |> fun (updated_expr_node, updated_acc_live_aliases) ->
        let updated_expr = { loc = expr.loc; typ = expr.typ; node = updated_expr_node } in
      (updated_expr :: acc_exprs, updated_acc_live_aliases))
    exprs
  |> fun (updated_exprs, updated_live_aliases) ->
  (Block (loc, type_expr, updated_exprs), updated_live_aliases)

and type_alias_liveness_loop_expr aliased_obj_name possible_aliases filter_linear_caps_fn
  live_aliases loop_expr =
  type_alias_liveness_block_expr aliased_obj_name possible_aliases filter_linear_caps_fn
    live_aliases loop_expr
  |> fun (updated_loop_expr, updated_live_aliases) ->
  if var_lists_are_equal live_aliases updated_live_aliases then
    (updated_loop_expr, updated_live_aliases)
  else
    type_alias_liveness_loop_expr aliased_obj_name possible_aliases filter_linear_caps_fn
      updated_live_aliases updated_loop_expr