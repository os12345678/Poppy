open! Core
open Ast_types

type identifier = Variable of Var_name.t [@@deriving sexp]

type expr = {
  loc : loc;
  node: expr_node
}
[@@deriving sexp]

and expr_node =
| Int                 of int
| Boolean             of bool
| Identifier          of identifier
| Constructor         of Class_name.t * type_expr option * constructor_arg list
| Let                 of type_expr option * Var_name.t * expr
| Assign              of identifier * expr  
| MethodApp           of Var_name.t * Method_name.t * expr list
| FunctionApp         of Function_name.t * expr list 
| If                  of expr * block_expr * block_expr
| While               of expr * block_expr
| For                 of expr * expr * expr * block_expr
| BinOp               of bin_op * expr * expr
| UnOp                of un_op * expr
[@@deriving sexp]

and block_expr = Block of loc * expr list
[@@deriving sexp]

and constructor_arg = ConstructorArg of Field_name.t * expr
[@@deriving sexp]

type function_definition = (*Function of Function_name.t * borrowed_ref option * type_expr * param list * block_expr*)
| Function of Function_name.t * param list * type_expr * block_expr
[@@deriving sexp]

(*Method of Method_name.t * borrowed_ref option * type_expr * param list * Capability_name.t list * block_expr*)
type method_defn =
| Method of
    Method_name.t
    * param list
    * type_expr
    * block_expr
    [@@deriving sexp]

type class_definition = (*Class of Class_name.t * generic_type option * Class_name.t option * capability list * field_defn list * method_defn list*)
| Class of
    Class_name.t
    * method_defn list
    [@@deriving sexp]

type program = Prog of class_definition list * function_definition list * block_expr
[@@deriving sexp]

let sexp_of_expressions expr =
  Sexp.List (List.map expr ~f:sexp_of_expr_node)