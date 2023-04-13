open! Core

type bin_op =
  | Plus
  | Minus
  | Times
  | Div
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq
  | And
  | Or
  | Xor
[@@deriving sexp_of]

type incr_decr_op = 
  | Incr of string
  | Decr of string
[@@deriving sexp_of]

type id_decl = Id of string
[@@deriving sexp_of]

type type_decl = Type of string
[@@deriving sexp_of]

type func_param = Param of id_decl * type_decl
[@@deriving sexp_of]

type proto = Prototype of id_decl * func_param list
[@@deriving sexp_of]

type expr =
  | IntLiteral of int
  | BoolLiteral of bool
  | Id of string
  | Type of type_decl
  | BinOp of bin_op * expr * expr
  | Not of expr
  | Print of string
  | Unit  
  | StringLiteral of string 
  | Lambda of func_param list * expr
  | Call of string * expr list
[@@deriving sexp_of]

type statement =
  | Let of (id_decl * type_decl) * expr
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | IncrDecr of string * incr_decr_op
  | For of string * int * expr * incr_decr_op * statement
  | Block of statement list
  | FuncDecl of proto * statement list
  | Return of expr
  | Expr of expr
[@@deriving sexp_of]


let sexp_of_statements statements =
  Sexp.List (List.map statements ~f:sexp_of_statement)