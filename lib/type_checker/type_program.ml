open Type_env
open Type_expr
open Type_struct

open Core.Result.Let_syntax
open Poppy_parser
open Core

let type_program (Ast.Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) = 
  let%bind typed_struct_defns = type_struct_defns struct_defns in
  let global_env = List.fold ~init:(Global (StructNameMap.empty, TraitNameMap.empty, MethodNameMap.empty, FunctionNameMap.empty)) 
                              ~f:add_struct_to_global 
                              struct_defns in
  (* print_global_env global_env; *)

  (* Type check the individual components of the program *)
  (* let%bind typed_struct_defns = Type_struct.type_struct_defns struct_defns global_env in *)
  (* let%bind typed_trait_defns = Type_trait.type_trait_defns trait_defns global_env in 
  let%bind typed_function_defns = Type_function.type_function_defns function_defns global_env in*)
  let%map typed_main_expr = type_block_expr struct_defns trait_defns method_defns function_defns main_expr global_env in

  Typed_ast.Prog (typed_struct_defns, [], [], [], typed_main_expr)