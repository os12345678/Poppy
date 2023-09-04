(* open Core

module A = Poppy_parser.Ast
module T = Poppy_parser.Ast_types
module F = Frontend_ir
module E = Desugar_env


let desugar_function_defn (function_defn: A.function_defn) : F.llvm_function_defn =
  match function_defn with
  | A.TFunction (function_signature, block_expr) ->
    let name = T.Function_name.to_string function_signature.name in
    let params = List.map ~f:E.desugar_param function_signature.params in
    let return_type = E.desugar_type function_signature.return_type in
    let (_, llvm_block_expr) = Desugar_expr.desugar_block_expr block_expr in
    F.LLVMFunction (name, params, return_type, llvm_block_expr) *)
