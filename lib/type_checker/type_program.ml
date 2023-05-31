open Core 
open Poppy_parser

let type_program (Ast.Prog (struct_defns, interface_defns, function_defns, main_expr)) = 
  let open Result in 
  Type_expr.type_block_expr struct_defns interface_defns function_defns main_expr []
  >>| fun (typed_main_expr, _) ->
    Typed_ast.Prog ([], [], [], typed_main_expr)