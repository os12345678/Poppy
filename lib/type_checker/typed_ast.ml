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

(* 
struct struct_name {
  field_name1: type_expr1,
  field_name2: type_expr2,
} 
*)
type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list
  * field_defn list
  [@@deriving sexp]

(* 
impl trait_name for struct_name {
  fn method_name(param1: type_expr1, param2: type_expr2) -> type_expr3 {
    body_expr
  }
} 
*)
type method_defn =
| TMethod of
    Trait_name.t
    * Struct_name.t option (* impl trait_name for struct_name *)
    * borrowed_ref option
    * type_expr
    * param list
    * block_expr
  [@@deriving sexp]
      
(*
trait trait_name {
  fn method_name(param1: type_expr1, param2: type_expr2) -> type_expr3;
}
*)
type trait_defn =
| TETrait of
    Trait_name.t
    * Method_name.t
    * borrowed_ref option
    * type_expr
    * param list
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
                struct_defn list
                * trait_defn list
                * method_defn list
                * function_defn list
                * block_expr
[@@deriving sexp]