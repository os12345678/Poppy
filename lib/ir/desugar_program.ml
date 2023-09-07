open Core

module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types
module D = Desugared_ast
module E = Desugar_env

let desugar_program (prog: T.program) : (D.dprogram , Error.t) Result.t =
  match prog with
  | T.Prog (structs, _traits, impls, functions, block_expr) ->
    let desugared_functions = List.map ~f:Desugar_functions.desugar_function_defn functions in
    let desugared_structs = List.map ~f:Desugar_structs.desugar_struct_defn structs in
    let desugared_impl = Desugar_impl.desugar_impl impls in
    let desugared_block = Desugar_expr.desugar_block block_expr in
    Ok {
      structs = desugared_structs;
      functions = desugared_impl @ desugared_functions;
      main = desugared_block;
    }

    (* let desugar_program (Prog(structs, traits, impls, funcs, main)) =
      {
        structs = List.map desugar_struct structs;
        functions = List.flatten [
                      List.map desugar_function funcs;
                      desugar_impl impls
                    ];
        main = desugar_block main;
      }
     *)
