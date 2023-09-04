open Core

module A = Poppy_parser.Ast
module T = Poppy_parser.Ast_types
module F = Frontend_ir
module E = Desugar_env

(* let desugar_program (prog: A.program) : F.llvm_program =
  match prog with
  | A.Prog (_structs, _traits, _impls, functions, block_expr) ->
    (* let llvm_structs = List.map ~f:Desugar_structs.desugar_struct_defn structs in
    let llvm_traits = List.map ~f:Desugar_traits.desugar_trait_defn traits in *)
    (* let llvm_impls = List.concat_map ~f:Desugar_methods.desugar_impl_defn impls in *)
    let llvm_functions = List.map ~f:Desugar_functions.desugar_function_defn functions in
    let (_, llvm_block_expr) = Desugar_expr.desugar_block_expr block_expr in
    F.LLVMProg ([], llvm_functions, llvm_block_expr) *)

let desugar_program (prog: A.program) : (F.llvm_program, Error.t) Result.t =
  match prog with
  | A.Prog (_structs, _traits, _impls, functions, block_expr) ->
    let llvm_functions = List.map ~f:Desugar_functions.desugar_function_defn functions in
    let (_, llvm_block_expr) = Desugar_expr.desugar_block_expr block_expr in
    Ok (F.LLVMProg ([], llvm_functions, llvm_block_expr))
