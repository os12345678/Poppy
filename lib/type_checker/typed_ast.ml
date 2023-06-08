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
| TFinishAsync         of loc * async_expr list * block_expr
| TIf                  of expr * block_expr * block_expr
| TWhile               of expr * block_expr
| TFor                 of expr * expr * expr * block_expr
| TBinOp               of bin_op * expr * expr
| TUnOp                of un_op * expr
| TNewStruct           of Struct_name.t * (Field_name.t * expr) list
| TAssignToInterface   of Var_name.t * expr
[@@deriving sexp]

and typed_identifier = 
  | TVariable of Var_name.t * type_expr
  | TObjField of Var_name.t * Field_name.t * type_expr
  [@@deriving sexp]

and block_expr = Block of loc * type_expr * expr list [@@deriving sexp]

and async_expr = AsyncExpr of block_expr [@@deriving sexp]

and constructor_arg = ConstructorArg of Field_name.t * expr [@@deriving sexp]

(* 
struct struct_name {
  field_name1: type_expr1,
  field_name2: type_expr2,
} 
*)
type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list (* mode[linear|threadlocal|read|locked|threadsafe|subordinate|encapsulated] * Capability_name.t list *)
  * field_defn list (* modifier[const|var] * type_expr[int|bool|void] * Field_name.t * Capability_name.t list *)
  [@@deriving sexp]

type method_signature = 
  | TMethodSignature of
    Method_name.t
    * borrowed_ref option (* borrowed[borrowed|]*)
    * Capability_name.t list
    * param list (* type_expr[int|bool|void] * Var_name.t * Capability_name.t list option * borrowed_ref option *)
    * type_expr (* ret type: int | bool | void *)
  [@@deriving sexp]

(* 
impl trait_name for struct_name {
  method_name borrowed linearCap(int:x read, int:y subordinate) -> void {
    body_expr
  }
} 
*)
type method_defn =
| TMethod of
    Trait_name.t
    * Struct_name.t option 
    * method_signature
    * block_expr
  [@@deriving sexp]
      
(*
trait trait_name {
  method_name borrowed linearCap(int:x read, int:y subordinate) -> void,
  method_name2 ...
}
*)
type trait_defn =
| TTrait of
    Trait_name.t
    * method_signature list
  [@@deriving sexp]

(*
fn function_name(param1: type_expr1, param2: type_expr2) -> type_expr3 {
  body_expr
}   
*)
type function_defn =
| TFunction of
    Function_name.t * borrowed_ref option * type_expr * param list * block_expr
    [@@deriving sexp]

type program = Prog of 
                (struct_defn list) 
                * (trait_defn list) 
                * (method_defn list) 
                * (function_defn list) 
                * block_expr
[@@deriving sexp]