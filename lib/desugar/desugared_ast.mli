module T = Poppy_parser.Ast_types
type dexpr = { loc : T.loc; typ : T.type_expr; node : dexpr_node; }
and dexpr_node =
    DIntLit of int
  | DBoolLit of bool
  | DStringLit of string
  | DBlockExpr of dexpr_node list
  | DVar of string
  | DAssign of string * dexpr_node
  | DBinOp of T.bin_op * dexpr_node * dexpr_node
  | DUnOp of T.un_op * dexpr_node
  | DCall of string * dexpr_node list
  | DIf of dexpr_node * dblock * dblock
  | DWhile of dexpr_node * dblock
  | DCreateThread of string * dexpr_node list
  | DJoinThread of dexpr_node
and dblock = dexpr list
val dexpr_of_sexp : Sexplib0.Sexp.t -> dexpr
val dexpr_node_of_sexp : Sexplib0.Sexp.t -> dexpr_node
val dblock_of_sexp : Sexplib0.Sexp.t -> dblock
val sexp_of_dexpr : dexpr -> Sexplib0.Sexp.t
val sexp_of_dexpr_node : dexpr_node -> Sexplib0.Sexp.t
val sexp_of_dblock : dblock -> Sexplib0.Sexp.t
type dfunction = {
  name : string;
  ret_type : T.type_expr;
  params : (T.type_expr * string) list;
  body : dblock;
}
val dfunction_of_sexp : Sexplib0.Sexp.t -> dfunction
val sexp_of_dfunction : dfunction -> Sexplib0.Sexp.t
type dstruct = { name : string; fields : (string * T.type_expr) list; }
val dstruct_of_sexp : Sexplib0.Sexp.t -> dstruct
val sexp_of_dstruct : dstruct -> Sexplib0.Sexp.t
type dprogram = {
  structs : dstruct list;
  functions : dfunction list;
  main : dblock;
}
val dprogram_of_sexp : Sexplib0.Sexp.t -> dprogram
val sexp_of_dprogram : dprogram -> Sexplib0.Sexp.t
