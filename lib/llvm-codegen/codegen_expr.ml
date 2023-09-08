(* open Llvm
(* open Poppy_parser *)
(* open Ast *)
(* open Core *)
(* open Poppy_type_checker *)
open Codegen_util

exception Codegen_error of string

module P = Poppy_parser.Ast

(* ##################### Module and Context Creation ######################## *)
let context = context
let builder = builder
let the_module = the_module
let named_values : (string, llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10

(* ############################ Codegen Block ############################### *)
(* let rec codegen_block (block: P.block_expr) : llvalue = 
  match block with
  | P.Block (_, exprs) -> List.iter exprs ~f:(codegen_expr)

 *)

(* ############################# Codegen Expr ############################### *)
(* and codegen_expr (expr: Ast.expr) (env: Type_env.env) : llvalue = 
  match expr.node with 
  | Ast.Int t -> const_int (i32_type context) t
  | Ast.Boolean b -> const_int (i1_type context) (if b then 1 else 0)
  | Ast.Identifier id -> 
 *)

 *)
