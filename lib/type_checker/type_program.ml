open Type_env
open Type_expr
open Type_struct

open Core.Result.Let_syntax
open Poppy_parser
open Core

let type_program (Ast.Prog (struct_defns, trait_defns, impl_defns, function_defns, main_expr)) = 
  (* Add the struct definitions to the global environment *)
  let%bind typed_struct_defns = type_struct_defns struct_defns in
  print_endline "###typed_struct_defns";
  let global_env = List.fold ~init:(Global (StructNameMap.empty, TraitNameMap.empty, MethodNameMap.empty, FunctionNameMap.empty, StructTraitMap.empty)) 
                              ~f:add_struct_to_global 
                              struct_defns in
  let global_env = List.fold ~init:global_env ~f:add_trait_to_global trait_defns in
  let global_env = List.fold ~init:global_env ~f:add_impl_to_global impl_defns in
  let global_env = List.fold ~init:global_env ~f:add_function_to_global function_defns in

  (* Type check the individual components of the program *)
  (* let%bind typed_trait_defns = Type_trait.type_trait_defns trait_defns global_env in  *)
  let%bind typed_function_defns = Type_function.type_function_defns global_env struct_defns trait_defns impl_defns function_defns in 
  print_endline "###typed_function_defns";
  let%bind typed_method_defns = Type_method.type_method_defns global_env struct_defns trait_defns impl_defns function_defns in
  print_endline "###typed_method_defns";

  (* Type check the main expression *)
  let%map typed_main_expr = type_block_expr struct_defns trait_defns impl_defns function_defns main_expr (add_block_scope global_env VarNameMap.empty) in
  print_endline "###typed_main_expr";
  Typed_ast.Prog (typed_struct_defns, [], typed_method_defns, typed_function_defns, typed_main_expr)