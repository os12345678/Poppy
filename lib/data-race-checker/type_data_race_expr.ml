open Core
open Poppy_type_checker.Typed_ast
open Type_read_capabilities
open Type_linear_capabilities
open Type_borrowing
open Type_async_capabilities
open Type_subord_capabilities
open Type_consume
open Aggregate_capability_access

let type_data_races_block_expr struct_defns _trait_defns impl_defns method_defns function_defns env (block_expr: block_expr) obj_vars_and_capabilities =
  let open Result in
  type_read_capabilities_block_expr block_expr
  |> type_subord_capabilities_block_expr env obj_vars_and_capabilities
  |> type_async_capabilities_block_expr env 
  |> type_linear_capabilities_block_expr env 
  >>= fun typed_block_expr ->
    Result.ignore_m (type_consume_block_expr env typed_block_expr [])
  >>= fun () ->
    type_function_forward_borrowing_block_expr method_defns function_defns env typed_block_expr
  >>= fun () ->
  Result.ignore_m (type_assign_borrowed_block_expr method_defns function_defns env typed_block_expr)
  >>= fun () ->
    Ok (aggregate_capability_accesses_block_expr struct_defns method_defns impl_defns function_defns env typed_block_expr)
    >>| fun (capability_access_aggregated_block_expr, _) -> capability_access_aggregated_block_expr