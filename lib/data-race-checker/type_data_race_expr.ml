open Core
open Poppy_type_checker.Typed_ast
open Type_read_capabilities
open Type_linear_capabilities
open Type_borrowing

let type_data_races_block_expr env (block_expr: block_expr) _obj_vars_and_capabilities =
  let open Result in 
    type_read_capabilities_block_expr block_expr
    |> type_linear_capabilities_block_expr env
    >>= fun typed_block_expr ->
      type_function_forward_borrowing_block_expr env typed_block_expr
    >>= fun () ->
      Result.ignore_m
      (type_assign_borrowed_block_expr env typed_block_expr)
    >>= fun () ->
      Ok typed_block_expr

  (* Ok (type_read_capabilities_block_expr block_expr) *)

  (* |> type_subord_capabilities_block_expr struct_defns trait_defns method_defns obj_vars_and_capabilities
  |> type_async_capabilities_block_expr struct_defns trait_defns method_defns *)
  (* |> type_linear_capabilities_block_expr struct_defns trait_defns method_defns
  >>= fun typed_block_expr ->
  Result.ignore_m (type_consume_block_expr struct_defns trait_defns method_defns typed_block_expr [])
  >>= fun () ->
  type_function_forward_borrowing_block_expr struct_defns trait_defns method_defns function_defns typed_block_expr
  >>= fun () ->
  Result.ignore_m
    (type_assign_borrowed_block_expr struct_defns trait_defns method_defns function_defns typed_block_expr)
  >>= fun () ->
  aggregate_capability_accesses_block_expr struct_defns trait_defns method_defns function_defns typed_block_expr
  |> fun (capability_access_aggregated_block_expr, _) ->
  ( if ignore_data_races then Ok ()
  else
    type_capabilities_constraints_block_expr struct_defns trait_defns method_defns function_defns
      capability_access_aggregated_block_expr )
  >>| fun () -> capability_access_aggregated_block_expr *)