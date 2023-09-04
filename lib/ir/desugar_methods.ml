open Core

module A = Poppy_parser.Ast
module T = Poppy_parser.Ast_types
module F = Frontend_ir
module E = Desugar_env


(* let desugar_method_defn (method_defn: A.method_defn) : F.llvm_function_defn =
  match method_defn with
  | A.TMethod (method_signature, block_expr) ->
    let name = E.mangle_name method_signature.name in
    let params = List.map ~f:E.desugar_param method_signature.params in
    let return_type = E.desugar_type method_signature.return_type in
    let (_, llvm_block_expr) = Desugar_expr.desugar_block_expr block_expr in
    LLVMFunction (name, params, return_type, llvm_block_expr) *)

let desugar_method_defn (trait_name: T.Trait_name.t) (struct_name: T.Struct_name.t) (method_defn: A.method_defn) : F.llvm_function_defn =
  match method_defn with
  | A.TMethod (method_signature, block_expr) ->
    let name = E.mangle_method_name trait_name struct_name method_signature.name in
    let params = List.map ~f:E.desugar_param method_signature.params in
    let return_type = E.desugar_type method_signature.return_type in
    let (_, llvm_block_expr) = Desugar_expr.desugar_block_expr block_expr in
    F.LLVMFunction (name, params, return_type, llvm_block_expr)

let desugar_impl_defn (impl_defn: A.impl_defn) : F.llvm_function_defn list =
  match impl_defn with
  | A.TImpl (trait_name, struct_name, method_defns) ->
    List.map ~f:(desugar_method_defn trait_name struct_name) method_defns
    
