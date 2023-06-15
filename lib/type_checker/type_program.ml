open Core.Result.Let_syntax
open Poppy_parser
open Type_env

let type_program (Ast.Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) = 
  let%bind typed_struct_defns = Type_struct.type_struct_defns struct_defns in
  let%bind typed_function_defns = Type_function.type_function_defns struct_defns trait_defns method_defns function_defns in
  let%map typed_main_expr = Type_expr.type_block_expr struct_defns trait_defns method_defns function_defns main_expr empty_context in
    Typed_ast.Prog (typed_struct_defns, [], [], typed_function_defns, typed_main_expr)
(*                                      /    |   
                                      /      |     
                                    trait  method  
*)