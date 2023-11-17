open Core
open Data_race_env
open Poppy_type_checker.Typed_ast

let update_capabilities_if_match names_to_match capability_filter_fn name capabilities =
  if elem_in_list name names_to_match then
    List.filter ~f:(capability_filter_fn capabilities) capabilities
  else capabilities

let update_matching_identifier_caps names_to_match capability_filter_fn id =
  match id with
  | TVariable (name, var_type, capabilities, maybeBorrowed) ->
      TVariable
        ( name,
          var_type,
          update_capabilities_if_match names_to_match capability_filter_fn name capabilities
        , maybeBorrowed )
  | TObjField (obj_struct, obj_name, field_type, field_name, capabilities, maybeBorrowed) ->
      TObjField
        ( obj_struct
        , obj_name
        , field_type
        , field_name
        , update_capabilities_if_match names_to_match capability_filter_fn obj_name
            capabilities
        , maybeBorrowed )

let rec update_matching_identifier_caps_expr names_to_match capability_filter_fn (expr: expr) =
  let update_matching_identifier_caps_expr_rec =
    update_matching_identifier_caps_expr names_to_match capability_filter_fn in
  let update_matching_identifier_caps_block_expr_rec =
    update_matching_identifier_caps_block_expr names_to_match capability_filter_fn in
  let update_var_modes_identifier_rec =
    update_matching_identifier_caps names_to_match capability_filter_fn in
  match expr.node with
  | TInt _ | TBoolean _ -> expr
  | TIdentifier id -> {expr with node = TIdentifier (update_var_modes_identifier_rec id)}
  | TBlockExpr block_expr -> {expr with node =
      TBlockExpr (update_matching_identifier_caps_block_expr_rec block_expr)}
  | TConstructor (struct_name, constructor_args) -> {expr with node =
      let updated_args =
        Core.List.map
          ~f:(fun (ConstructorArg (field_name, expr)) ->
            ConstructorArg
              (field_name, update_matching_identifier_caps_expr_rec expr))
          constructor_args in
      TConstructor (struct_name, updated_args)}
  | TLet (type_expr, names_to_match, bound_expr) -> {expr with node =
      TLet
        ( type_expr
        , names_to_match
        , update_matching_identifier_caps_expr_rec bound_expr )}
  | TAssign (id, assigned_expr) -> {expr with node =
      TAssign
        ( update_var_modes_identifier_rec id
        , update_matching_identifier_caps_expr_rec assigned_expr )}
  | TConsume id -> {expr with node = TConsume (update_var_modes_identifier_rec id)}
  | TMethodApp (obj_struct, struct_name, trait_name, method_name, obj_capabilities, args) -> {expr with node =
      TMethodApp
        ( obj_struct
        , struct_name
        , trait_name
        , method_name
        , obj_capabilities
        , List.map ~f:update_matching_identifier_caps_expr_rec args )}
  | TFunctionApp (func_name, args) -> {expr with node =
      TFunctionApp
        ( func_name
        , List.map ~f:update_matching_identifier_caps_expr_rec args )}
  | TPrintf (format_str, args) -> {expr with node =
      TPrintf (format_str, List.map ~f:update_matching_identifier_caps_expr_rec args)}
  | TFinishAsync (async_exprs, curr_thread_free_vars, curr_thread_expr) -> {expr with node =
      TFinishAsync
        ( List.map
            ~f:(fun (AsyncExpr (free_objs, expr)) ->
              AsyncExpr (free_objs, update_matching_identifier_caps_block_expr_rec expr))
            async_exprs, curr_thread_free_vars
        , update_matching_identifier_caps_block_expr_rec curr_thread_expr )}
  | TIf (cond_expr, then_expr, else_expr) -> {expr with node =
      TIf
        ( update_matching_identifier_caps_expr_rec cond_expr
        , update_matching_identifier_caps_block_expr_rec then_expr
        , update_matching_identifier_caps_block_expr_rec else_expr )}
  | TWhile (cond_expr, loop_expr) -> {expr with node =
      TWhile
        ( update_matching_identifier_caps_expr_rec cond_expr
        , update_matching_identifier_caps_block_expr_rec loop_expr )}
  | TBinOp (binop, expr1, expr2) -> {expr with node =
      TBinOp
        ( binop
        , update_matching_identifier_caps_expr_rec expr1
        , update_matching_identifier_caps_expr_rec expr2 )}
  | TUnOp (unop, expr) -> {expr with node =
      TUnOp (unop, update_matching_identifier_caps_expr_rec expr)}
         

and update_matching_identifier_caps_block_expr names_to_match capability_filter_fn
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs =
    Core.List.map
      ~f:(update_matching_identifier_caps_expr names_to_match capability_filter_fn)
      exprs in
  Block (loc, type_block_expr, updated_exprs)