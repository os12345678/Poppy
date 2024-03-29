(* Decorate original AST with additional type information *)

open! Core
open Poppy_parser.Ast_types

type obj_var_and_capabilities = Var_name.t * Struct_name.t * capability list [@@deriving sexp]

type expr = {
  loc : loc;
  typ : type_expr; (* additional type information *)
  node: expr_node
}
[@@deriving sexp]

and expr_node =
| TInt                 of int
| TBoolean             of bool
| TIdentifier          of typed_identifier
| TLet                 of type_expr option * Var_name.t * expr
| TBlockExpr           of block_expr (* used to interconvert with block expr *)
| TAssign              of typed_identifier * expr  
| TConstructor         of Struct_name.t * constructor_arg list
| TConsume            of typed_identifier
| TMethodApp           of Var_name.t * Struct_name.t * Trait_name.t * Method_name.t * capability list * expr list
| TFunctionApp         of Function_name.t * expr list 
| TIf                  of expr * block_expr * block_expr
| TWhile               of expr * block_expr
| TPrintf              of string * expr list 
| TBinOp               of bin_op * expr * expr
| TUnOp                of un_op * expr
| TFinishAsync         of async_expr list * obj_var_and_capabilities list * block_expr
[@@deriving sexp]

and typed_identifier = 
  | TVariable of Var_name.t * type_expr * capability list * borrowed_ref option
  | TObjField of  
  Struct_name.t (* struct of the object*)
  * Var_name.t
  * type_expr
  * Field_name.t (*type of field *)
  * capability list
  * borrowed_ref option
  [@@deriving sexp]

and block_expr = Block of loc * type_expr * expr list [@@deriving sexp]

(* type is of the final expr in block *)
and async_expr = AsyncExpr of obj_var_and_capabilities list * block_expr [@@deriving sexp]

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
    function_signature * block_expr
    [@@deriving sexp]

type program = Prog of 
                (struct_defn list) 
                * (trait_defn list) 
                * (impl_defn list) 
                * (function_defn list) 
                * block_expr
[@@deriving sexp]