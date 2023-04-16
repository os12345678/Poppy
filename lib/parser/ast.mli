type bin_op =
    Plus
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
val sexp_of_bin_op : bin_op -> Sexplib0.Sexp.t
type incr_decr_op = Incr of string | Decr of string
val sexp_of_incr_decr_op : incr_decr_op -> Sexplib0.Sexp.t
type id_decl = Id of string
val sexp_of_id_decl : id_decl -> Sexplib0.Sexp.t
type typ = Int | Bool | Void | String
val sexp_of_typ : typ -> Sexplib0.Sexp.t
val string_to_typ : string -> typ
type type_decl = Type of typ
val sexp_of_type_decl : type_decl -> Sexplib0.Sexp.t
type func_param = Param of id_decl * type_decl
val sexp_of_func_param : func_param -> Sexplib0.Sexp.t
type expr =
    Expr of expr
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
val sexp_of_expr : expr -> Sexplib0.Sexp.t
type statement =
    Let of (id_decl * type_decl) * expr
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | IncrDecr of string * incr_decr_op
  | For of string * int * expr * incr_decr_op * statement
  | Block of statement list
  | FuncDecl of id_decl * func_param list * type_decl * statement list
  | Return of expr
  | Expr of expr
val sexp_of_statement : statement -> Sexplib0.Sexp.t
val sexp_of_statements : statement list -> Sexplib0.Sexp.t
