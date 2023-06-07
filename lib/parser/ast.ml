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
| Constructor         of Var_name.t * Struct_name.t * constructor_arg list
| MethodApp           of Var_name.t * Method_name.t * expr list
| FunctionApp         of Function_name.t * expr list 
| FinishAsync         of loc * async_expr list * block_expr
| If                  of expr * block_expr * block_expr
| While               of expr * block_expr
| For                 of expr * expr * expr * block_expr
| BinOp               of bin_op * expr * expr
| UnOp                of un_op * expr
(* | NewStruct           of Struct_name.t * (Field_name.t * expr) list *)
(* | AssignToInterface   of Var_name.t * expr *)
[@@deriving sexp]

and block_expr = Block of loc * expr list [@@deriving sexp]

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
  * capability list
  * field_defn list
  [@@deriving sexp]

(* 
impl trait_name for struct_name {
  method_name(param1: type_expr1, param2: type_expr2) -> type_expr3 {
    body_expr
  }
} 
*)
type method_defn =
| TMethod of
    Trait_name.t
    * Struct_name.t option (* impl trait_name for struct_name *)
    * Method_name.t
    * borrowed_ref option
    * type_expr
    * param list
    * block_expr
  [@@deriving sexp]
      
(*
trait trait_name {
  method_name(param1: type_expr1, param2: type_expr2) -> type_expr3;
}
*)
type trait_defn =
| TTrait of
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
                (struct_defn list) 
                * (trait_defn list) 
                * (method_defn list) 
                * (function_defn list) 
                * block_expr
[@@deriving sexp]