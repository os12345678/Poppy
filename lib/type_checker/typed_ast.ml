(* Decorate original AST with additional type information *)

open! Core
open Poppy_parser.Ast_types

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
| TAssign              of typed_identifier * expr  
| TConstructor         of Var_name.t * Struct_name.t * constructor_arg list
| TMethodApp           of Var_name.t * Method_name.t * expr list
| TFunctionApp         of Function_name.t * expr list 
| TMutexConstructor    of type_expr * expr
| TThread              of expr
| TFinishAsync         of loc * async_expr list * block_expr
| TIf                  of expr * block_expr * block_expr
| TWhile               of expr * block_expr
| TFor                 of expr * expr * expr * block_expr
| TBinOp               of bin_op * expr * expr
| TUnOp                of un_op * expr
| TNewStruct           of Struct_name.t * (Field_name.t * expr) list
| TAssignToInterface   of Var_name.t * expr
| TLock                 of expr
| TUnlock               of expr
[@@deriving sexp]

and typed_identifier = 
  | TVariable of Var_name.t * type_expr
  | TObjField of Var_name.t * Field_name.t * type_expr
  [@@deriving sexp]

and block_expr = Block of loc * type_expr * expr list [@@deriving sexp]

and async_expr = AsyncExpr of block_expr [@@deriving sexp]

and constructor_arg = ConstructorArg of Field_name.t * expr [@@deriving sexp]

type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list
  * field_defn list
  [@@deriving sexp]

type method_signature = 
  | TMethodSignature of
    Method_name.t
    * borrowed_ref option 
    * Capability_name.t list
    * param list 
    * type_expr 
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
    Function_name.t * borrowed_ref option * type_expr * param list * block_expr
    [@@deriving sexp]

type program = Prog of 
                (struct_defn list) 
                * (trait_defn list) 
                * (impl_defn list) 
                * (function_defn list) 
                * block_expr
[@@deriving sexp]