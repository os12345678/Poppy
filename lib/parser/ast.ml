open! Core

(* Heper Functions *)


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

type typ = 
| Int
| Bool
| Void
| String
[@@deriving sexp_of]


let string_to_typ s = match s with
  | "int" -> Int
  | "bool" -> Bool
  | "void" -> Void
  | "string" -> String
  | _ -> raise (Printf.sprintf "Unknown type: %s" s |> Failure)

type type_decl = Type of typ
[@@deriving sexp_of]

type func_param = Param of id_decl * type_decl
[@@deriving sexp_of]


type expr =
  | Expr of expr
  | IntLiteral of int
  | BoolLiteral of bool
  | VoidType 
  | StringType of string
  | Id of string
  | BinOp of bin_op * expr * expr
  | Not of expr
  | Print of string
  | Unit  
  | StringLiteral of string 
  | Lambda of func_param list * expr
  | Call of string * expr list
[@@deriving sexp_of]

type mutexId = MutexId of string
[@@deriving sexp_of]

type statement =
  | Let of (id_decl * type_decl) * expr
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | IncrDecr of string * incr_decr_op
  | For of string * int * expr * incr_decr_op * statement
  | Block of statement list
  | FuncDecl of id_decl * func_param list * type_decl * statement list
  | MainFunc of statement list
  | Thread of statement
  | Return of expr
  | Expr of expr
  | MutexDeclaration of mutexId * type_decl
  | MutexLock of mutexId
  | MutexUnlock of mutexId
[@@deriving sexp_of]

let sexp_of_statements statements =
  Sexp.List (List.map statements ~f:sexp_of_statement)