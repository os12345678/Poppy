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
| MethodApp           of Var_name.t * Method_name.t * expr list
| FunctionApp         of Function_name.t * expr list 
| FinishAsync         of loc * async_expr list * block_expr
| If                  of expr * block_expr * block_expr
| While               of expr * block_expr
| For                 of expr * expr * expr * block_expr
| BinOp               of bin_op * expr * expr
| UnOp                of un_op * expr
| NewStruct           of Struct_name.t * (Field_name.t * expr) list
| AssignToInterface   of Var_name.t * expr
[@@deriving sexp]

and block_expr = Block of loc * expr list [@@deriving sexp]

and async_expr = AsyncExpr of block_expr [@@deriving sexp]

(* Struct definitions consist of the struct name, its fields, and its methods *)
type struct_defn = 
  | TStruct of 
  Struct_name.t 
  * capability list
  * field_defn list
  * method_defn list  (* Method definitions associated with the struct *)
  [@@deriving sexp]
      
(* Interface definitions consist of the interface name and its methods *)
and interface_defn = 
  | TInterface of 
  Interface_name.t 
  * interface_method_defn list  (* Include method names in the signature *)
  [@@deriving sexp]
      
(* Method defn consists of the method name, return type (and whether it returns a borrowed
   ref), the list of params, and the capabilities used and the body expr of the function *)
and method_defn =
| TMethod of
    Method_name.t
    * borrowed_ref option
    * type_expr
    * param list
    * block_expr
  [@@deriving sexp]

(* Interface method defn only contains the method signature of method_defn *)
and interface_method_defn =
| TInterfaceMethod of
    Method_name.t
    * borrowed_ref option
    * type_expr
    * param list
  [@@deriving sexp]

(* Function defn consists of the function name, return type, the list of params, and the
   body expr of the function *)
type function_defn =
| TFunction of
    Function_name.t * borrowed_ref option * type_expr * param list * block_expr
    [@@deriving sexp]

type program = Prog of 
                struct_defn list 
                * interface_defn list 
                * function_defn list 
                * block_expr
[@@deriving sexp]

let sexp_of_expressions expr =
  Sexp.List (List.map expr ~f:sexp_of_expr_node)