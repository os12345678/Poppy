(* (* Original AST simplified and contains LLVM IR specific constructs i.e load, 
store, call, etc.

Key Changes
-----------
Identifier Types: Reduced to simple variables and fields. Complex types like 
methods and traits are not directly representable in LLVM IR.

Expressions: Removed high-level constructs like Let, Assign, Constructor, 
MethodApp, etc. These will be lowered into simpler constructs like LLVMLoad, 
LLVMStore, and LLVMCall.

Function Definitions: Simplified to only include the function name, parameter 
types and names, and the block expression.

Program Structure: Removed traits and impls. These are high-level 
constructs that need to be lowered into simpler constructs before generating LLVM IR.

Locks and Threads: Kept these as they can be directly mapped to LLVM's atomic 
instructions.

Binary and Unary Operations: Kept these as they can be directly mapped to LLVM 
instructions.

Control Flow: Kept If, While as they can be directly mapped to LLVM's branching 
instructions.
*)

open! Core

module T = Poppy_parser.Ast_types

type llvm_identifier = 
  | LVariable of string * mutex_state
  | LField of string * string * mutex_state
  [@@deriving sexp]

type llvm_expr = {
  loc : T.loc;
  l_type: string;
  l_node: llvm_expr_node
}
[@@deriving sexp]

and llvm_expr_node =
  | LLVMInt          of int
  | LLVMBool         of bool
  | LLVMIdentifier   of llvm_identifier
  | LLVMLoad         of llvm_identifier
  | LLVMStore        of llvm_identifier * llvm_expr
  | LLVMCall         of string * llvm_expr list
  | LLVMIf           of llvm_expr * llvm_block_expr * llvm_block_expr
  | LLVMWhile        of llvm_expr * llvm_block_expr
  | LLVMBinOp        of string * llvm_expr * llvm_expr
  | LLVMUnOp         of string * llvm_expr
  | LLVMLock         of llvm_identifier
  | LLVMUnlock       of llvm_identifier
  | LLVMThread       of string * llvm_block_expr
  [@@deriving sexp]

and llvm_block_expr = LLVMBlock of T.loc * llvm_expr list [@@deriving sexp]

type llvm_struct_field = {
  field_name: string;
  field_type: string;
  field_mutex_state: mutex_state;
}
[@@deriving sexp]

type llvm_struct_defn = {
  struct_name: string;
  struct_fields: llvm_struct_field list;
}
[@@deriving sexp]

type llvm_function_defn =
  | LLVMFunction of
      string (* function name *)
      * (string * string) list (* parameter types and names *)
      * string (* return type *)
      * llvm_block_expr
  [@@deriving sexp]

type llvm_program = LLVMProg of 
                    llvm_struct_defn list
                    * llvm_function_defn list 
                    * llvm_block_expr
[@@deriving sexp] *)