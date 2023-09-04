open Core

module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types
module D = Desugared_ast
module E = Desugar_env

let desugar_program (prog: T.program) : (D.dprogram , Error.t) Result.t =
  match prog with
  | T.Prog (_structs, _traits, _impls, _functions, block_expr) ->
    (* let llvm_functions = List.map ~f:Desugar_functions.desugar_function_defn functions in *)
    let desugared_block = Desugar_expr.desugar_block block_expr in
    Ok {
      D.structs = []; (* You can fill this in once you implement desugar_structs *)
      D.functions = []; (* You can fill this in once you implement desugar_functions *)
      D.main = desugared_block;
    }
