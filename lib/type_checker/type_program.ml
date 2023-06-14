(* open Core  *)
open Poppy_parser

let type_program (Ast.Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) = 
  (* let open Result in  *)
  let open Core.Result.Let_syntax in
  (* Type_struct.type_struct_defns struct_defns
  >>= fun typed_struct_defns -> *)
  let%map typed_main_expr = Type_expr.type_block_expr struct_defns trait_defns method_defns function_defns main_expr Type_env.empty_context in
    Typed_ast.Prog ([], [], [], [], typed_main_expr)
(*                                      /    |    \
                                      /      |     \
                                    trait  method  function
*)