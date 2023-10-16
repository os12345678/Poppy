open Type_env
open Type_expr
open Type_struct

open Core.Result.Let_syntax
open Poppy_parser
open Core

(* Add the given definitions to the global environment *)
let add_definitions_to_global ~init ~f defns = 
  List.fold ~init ~f defns

(* Type check the individual components of the program *)
let type_check_definitions global_env struct_defns trait_defns impl_defns function_defns = 
  let%bind typed_trait_defns = Type_trait.type_trait_defns trait_defns in 
  let%bind typed_function_defns = Type_function.type_function_defns global_env struct_defns trait_defns impl_defns function_defns in 
  let%map typed_method_defns = Type_method.type_method_defns global_env struct_defns trait_defns impl_defns function_defns in
  (typed_trait_defns, typed_function_defns, typed_method_defns)

(* Type check the main expression *)
let type_check_main_expr global_env struct_defns trait_defns impl_defns function_defns main_expr = 
  type_block_expr struct_defns trait_defns impl_defns function_defns main_expr (add_block_scope global_env VarNameMap.empty)

let type_program (Ast.Prog (struct_defns, trait_defns, impl_defns, function_defns, main_expr)) = 
  let%bind typed_struct_defns = type_struct_defns struct_defns in
  let initial_global_env = init_global_scope () in 
  let global_env = add_definitions_to_global ~init:initial_global_env ~f:add_struct_to_global struct_defns in
  let global_env = add_definitions_to_global ~init:global_env ~f:add_trait_to_global trait_defns in
  let global_env = add_definitions_to_global ~init:global_env ~f:add_impl_to_global impl_defns in
  let global_env = add_definitions_to_global ~init:global_env ~f:add_function_to_global function_defns in

  let%bind typed_trait_defns, typed_function_defns, typed_method_defns = type_check_definitions global_env struct_defns trait_defns impl_defns function_defns in
  let%map (typed_main_expr, _) = type_check_main_expr global_env struct_defns trait_defns impl_defns function_defns main_expr in

  (global_env, Typed_ast.Prog (typed_struct_defns, typed_trait_defns, typed_method_defns, typed_function_defns, typed_main_expr))
