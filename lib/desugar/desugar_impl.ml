open Core

module T = Poppy_type_checker.Typed_ast
module Typ = Poppy_parser.Ast_types
module D = Desugared_ast


(* for each impl_defn in program:
    trait_name = impl_defn.trait_name
    struct_name = impl_defn.struct_name
    for each method_defn in impl_defn:
        dfunction = {
            name: mangle(struct_name, method_name),
            ret_type: method_signature.return_type,
            params: method_signature.params,
            body: method_defn.block_expr
        }
        add dfunction to dprogram.functions 
*)

let rec desugar_impl impls =
  List.concat (List.map ~f: desugar_impl_single impls)

and desugar_impl_single (T.TImpl(trait_name, struct_name, methods)) =
  List.map ~f:(desugar_method struct_name trait_name) methods

and desugar_method (struct_name: Typ.Struct_name.t) (trait_name: Typ.Trait_name.t) 
                    (T.TMethod(signature, block_expr)) : D.dfunction =
  let this_param = Typ.Param ( 
    Typ.TEStruct(struct_name), 
    Typ.Var_name.of_string "this",
    None, 
    None
  ) in
  {
    name = Desugar_env.mangle_impl trait_name struct_name signature.name;
    ret_type = signature.return_type;
    params = Desugar_env.desugar_param(this_param) :: (List.map ~f: Desugar_env.desugar_param (signature.params));
    body = Desugar_expr.desugar_block block_expr;
  }

