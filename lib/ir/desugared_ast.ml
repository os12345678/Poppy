open! Core

module T = Poppy_parser.Ast_types

type dexpr = {
  loc : T.loc;
  typ : T.type_expr;
  node: string;
}

and dexpr_node =
| DIntLit              of int
| DBoolLit             of bool
| DVar                 of string
| DAssign              of string * dexpr_node
| DBinOp               of T.bin_op * dexpr_node * dexpr_node
| DUnOp                of T.un_op * dexpr_node
| DCall                of string * dexpr_node list
| DIf                  of dexpr_node * dblock * dblock
| DWhile               of dexpr_node * dblock
| DCreateThread        of string (* function name *) * dexpr_node (* argument *)
| DJoinThread          of dexpr_node (* thread identifier *)

and dblock = dexpr list
