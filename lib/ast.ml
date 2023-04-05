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

type id_decl = Id of string
[@@deriving sexp_of]

type type_decl = Type of string
[@@deriving sexp_of]

type expr =
  | Int of int
  | Bool of bool
  | Id of string
  | Type of type_decl
  | BinOp of bin_op * expr * expr
  | Not of expr
  | Incr of string
  | Decr of string
  | Print of string
  | Builtin of string * expr list
  | Unit  
  | StringLiteral of string 
[@@deriving sexp_of]

type func_param = Param of id_decl * type_decl
[@@deriving sexp_of]

type statement =
  | Let of (id_decl * type_decl) * expr
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | For of string * int * expr * expr * statement
  | Block of statement list
  | FuncDecl of id_decl * func_param list * statement list
  | Return of expr
  | Expr of expr
[@@deriving sexp_of]

let sexp_of_statements statements =
  Sexp.List (List.map statements ~f:sexp_of_statement)