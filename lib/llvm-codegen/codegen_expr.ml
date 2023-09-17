(* open Llvm *)
open Codegen_util
(* open Desugar.Desugared_ast *)

exception Codegen_error of string

module D = Desugar.Desugared_ast
module U = Codegen_util
module A = Poppy_parser.Ast_types
module St = Symbol_table

(* ##################### Module and Context Creation ######################## *)
let context = context
let builder = builder
let the_module = the_module

let () = declare_thread_functions the_module