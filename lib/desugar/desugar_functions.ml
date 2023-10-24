open Core

module T = Poppy_type_checker.Typed_ast
module Typ = Poppy_parser.Ast_types
module D = Desugared_ast
module E = Desugar_env

(* 
for each function_defn in program:
    dfunction = {
        name: function_name,
        ret_type: function_signature.return_type,
        params: function_signature.params,
        body: function_defn.block_expr
    }
    add dfunction to dprogram.functions   
*)

let desugar_function_defn (function_defn: T.function_defn): D.dfunction =
  match function_defn with
  | T.TFunction (function_signature, block_expr) ->
    let name = Typ.Function_name.to_string function_signature.name in
    let params = List.map ~f:E.desugar_param function_signature.params in
    let ret_type = function_signature.return_type in
    let body = Desugar_expr.desugar_block block_expr in
    { name; ret_type; params; body }
