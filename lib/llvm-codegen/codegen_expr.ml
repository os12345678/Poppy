(* open Llvm
open Codegen_util
(* open Desugar.Desugared_ast *)

exception Codegen_error of string

module L = Llvm
module D = Desugar.Desugared_ast
module U = Codegen_util
module T = Poppy_parser.Ast_types
module St = Symbol_table

(* ############################# LLVM setup ################################# *)
module St = struct

  let rec codegen_expr (builder: llbuilder) (symboltable: St.llvm_symbol_table) (expr: D.dexpr) : llvalue = 
    match expr.node with
    | DVar name -> 
      (match lookup_symbol name with 
      )
    | _ -> raise (Codegen_error "not implemented")

  (* let codegen_function_def (f: D.dfunction) (symbol_table: St.llvm_symbol_table) : Llvm.llmodule =


  let codegen_struct bulider symboltable struct_def : llmodule = 

     *)

  (* Generate code for structs
  let codegen_struct builder symboltable struct_def =  *)
  let codegen_ast dprogram symboltable : llmodule = 
    let context = Llvm.global_context () in
    let the_module = Llvm.create_module context "Poppy JIT" in
    let builder = Llvm.builder context in

    (* Generate code for all structs *)
    (* Generate code for all functions *)
    (* Generate code for the main block *)
    (* List.iter dprogram.main ~f:(codegen_block builder symboltable); *)

  the_module
end *)