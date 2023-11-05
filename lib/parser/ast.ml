open! Core
open Ast_types

type identifier = 
  | Variable of Var_name.t 
  | ObjField of Var_name.t * Field_name.t
  [@@deriving sexp]

type expr = {
  loc : loc;
  node: expr_node
}
[@@deriving sexp]

and expr_node =
| Int                 of int
| Boolean             of bool
| Identifier          of identifier
| Let                 of type_expr option * Var_name.t * expr
| Assign              of identifier * expr  
| Consume             of identifier
| Constructor         of Var_name.t * Struct_name.t * constructor_arg list
| MethodApp           of Var_name.t * Method_name.t * expr list
| FunctionApp         of Function_name.t * expr list 
| If                  of expr * block_expr * block_expr
| While               of expr * block_expr
| For                 of expr * expr * expr * block_expr
| Printf              of string * expr list
| BinOp               of bin_op * expr * expr
| UnOp                of un_op * expr
| FinishAsync         of async_expr list * block_expr
[@@deriving sexp]

and block_expr = Block of loc * expr list [@@deriving sexp]

and async_expr = AsyncExpr of block_expr [@@deriving sexp]

and constructor_arg = ConstructorArg of Field_name.t * expr [@@deriving sexp]

type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list
  * field_defn list
  [@@deriving sexp]

type method_defn =
  | TMethod of
    method_signature
    * block_expr
  [@@deriving sexp]

type impl_defn = 
  | TImpl of
    Trait_name.t
    * Struct_name.t
    * method_defn list
  [@@deriving sexp]
      
type trait_defn =
| TTrait of
    Trait_name.t
    * method_signature list
  [@@deriving sexp]

type function_defn =
| TFunction of
    function_signature 
    * block_expr
    [@@deriving sexp]

type program = Prog of 
                (struct_defn list) 
                * (trait_defn list) 
                * (impl_defn list) 
                * (function_defn list) 
                * block_expr
[@@deriving sexp]