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
[@@deriving sexp_of]

type expr =
  | Int of int
  | Bool of bool
  | Id of string
  | BinOp of bin_op * expr * expr
  | Not of expr
[@@deriving sexp_of]

type statement =
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | FuncDecl of string * string list * statement
  | Block of statement list
[@@deriving sexp_of]
