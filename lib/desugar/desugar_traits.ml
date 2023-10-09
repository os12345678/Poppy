(* open Core

module T = Poppy_type_checker.Typed_ast
module Typ = Poppy_parser.Ast_types
module D = Desugared_ast *)

(* 
for each trait_defn in program:
    for each method_signature in trait_defn:
        -- this would normally define a method contract without a body.
        -- Since we're lowering this for LLVM IR generation,
        -- no direct representation is needed in the desugared version.
        -- Instead, we'll handle these when we process impl_defn. 
*)

