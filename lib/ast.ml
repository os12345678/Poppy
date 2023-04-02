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

type expr =
  | Int of int
  | Bool of bool
  | Id of string
  | BinOp of bin_op * expr * expr
  | Not of expr
  | Incr of string
  | Decr of string
  | Print of string
  | List of expr list
  | Builtin of string * expr list (*function name and argument*)
[@@deriving sexp_of]

type func_param = string * string
[@@deriving sexp_of]

type statement =
  | Let of string * expr
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | For of string * int * expr * expr * statement
  | Block of statement list
  | FuncDecl of string * func_param list * statement list
  | Return of expr option
[@@deriving sexp_of]
