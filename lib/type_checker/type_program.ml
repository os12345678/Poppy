open Core 
open Poppy_parser

let type_program (Ast.Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) = 
  let open Result in 
  Type_struct.type_struct_defns struct_defns
  >>= fun typed_struct_defns ->
  Type_expr.type_block_expr struct_defns trait_defns method_defns function_defns main_expr Type_env.empty_context
  >>| fun (typed_main_expr, _) ->
    Typed_ast.Prog (typed_struct_defns, [], [], [], typed_main_expr)
(*                                      /    |    \
                                      /      |     \
                                    trait  method  function
*)