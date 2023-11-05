open! Core

module T = Poppy_parser.Ast_types

type dexpr = {
  loc : T.loc;
  typ : T.type_expr;
  node: dexpr_node;
}
[@@deriving sexp]

and dexpr_node =
| DIntLit              of int
| DBoolLit             of bool
| DStringLit           of string
| DBlockExpr           of dexpr_node list
| DVar                 of string
| DAssign              of string * dexpr_node
| DBinOp               of T.bin_op * dexpr_node * dexpr_node
| DUnOp                of T.un_op * dexpr_node
| DCall                of string * dexpr_node list
| DIf                  of dexpr_node * dblock * dblock
| DWhile               of dexpr_node * dblock
| DCreateThread        of string * dexpr_node list (* argument *)
| DJoinThread          of dexpr_node (* thread identifier *)

and dblock = dexpr list [@@deriving sexp]

type dfunction = {
  name: string;
  ret_type: T.type_expr;
  params: (T.type_expr * string) list;
  body: dblock;
} [@@deriving sexp]

type dstruct = {
  name: string;
  fields: (string * T.type_expr) list;
} [@@deriving sexp]

type dprogram = {
  structs: dstruct list;
  functions: dfunction list;
  main: dblock;
} [@@deriving sexp]
