type expr = {
  loc : Poppy_parser.Ast_types.loc;
  typ : Poppy_parser.Ast_types.type_expr;
  node : expr_node;
}
and expr_node =
    TInt of int
  | TBoolean of bool
  | TIdentifier of typed_identifier
  | TLet of Poppy_parser.Ast_types.type_expr option *
      Poppy_parser.Ast_types.Var_name.t * expr
  | TAssign of typed_identifier * expr
  | TMethodApp of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.Method_name.t * expr list
  | TFunctionApp of Poppy_parser.Ast_types.Function_name.t * expr list
  | TFinishAsync of Poppy_parser.Ast_types.loc * async_expr list * block_expr
  | TIf of expr * block_expr * block_expr
  | TWhile of expr * block_expr
  | TFor of expr * expr * expr * block_expr
  | TBinOp of Poppy_parser.Ast_types.bin_op * expr * expr
  | TUnOp of Poppy_parser.Ast_types.un_op * expr
  | TNewStruct of Poppy_parser.Ast_types.Struct_name.t *
      (Poppy_parser.Ast_types.Field_name.t * expr) list
  | TAssignToInterface of Poppy_parser.Ast_types.Var_name.t * expr
and typed_identifier =
    TVariable of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.type_expr
  | TObjField of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.Field_name.t * Poppy_parser.Ast_types.type_expr
and block_expr =
    Block of Poppy_parser.Ast_types.loc * Poppy_parser.Ast_types.type_expr *
      expr list
and async_expr = AsyncExpr of block_expr
val expr_of_sexp : Sexplib0.Sexp.t -> expr
val expr_node_of_sexp : Sexplib0.Sexp.t -> expr_node
val typed_identifier_of_sexp : Sexplib0.Sexp.t -> typed_identifier
val block_expr_of_sexp : Sexplib0.Sexp.t -> block_expr
val async_expr_of_sexp : Sexplib0.Sexp.t -> async_expr
val sexp_of_expr : expr -> Sexplib0.Sexp.t
val sexp_of_expr_node : expr_node -> Sexplib0.Sexp.t
val sexp_of_typed_identifier : typed_identifier -> Sexplib0.Sexp.t
val sexp_of_block_expr : block_expr -> Sexplib0.Sexp.t
val sexp_of_async_expr : async_expr -> Sexplib0.Sexp.t
type interface_method_defn =
    TInterfaceMethod of Poppy_parser.Ast_types.Method_name.t *
      Poppy_parser.Ast_types.borrowed_ref option *
      Poppy_parser.Ast_types.type_expr * Poppy_parser.Ast_types.param list
val interface_method_defn_of_sexp : Sexplib0.Sexp.t -> interface_method_defn
val sexp_of_interface_method_defn : interface_method_defn -> Sexplib0.Sexp.t
type interface_defn =
    TInterface of Poppy_parser.Ast_types.Interface_name.t *
      interface_method_defn list
val interface_defn_of_sexp : Sexplib0.Sexp.t -> interface_defn
val sexp_of_interface_defn : interface_defn -> Sexplib0.Sexp.t
type method_defn =
    TMethod of Poppy_parser.Ast_types.Method_name.t *
      Poppy_parser.Ast_types.borrowed_ref option *
      Poppy_parser.Ast_types.type_expr * Poppy_parser.Ast_types.param list *
      block_expr
val method_defn_of_sexp : Sexplib0.Sexp.t -> method_defn
val sexp_of_method_defn : method_defn -> Sexplib0.Sexp.t
type struct_defn =
    TStruct of Poppy_parser.Ast_types.Struct_name.t *
      Poppy_parser.Ast_types.capability list *
      Poppy_parser.Ast_types.field_defn list * method_defn list
val struct_defn_of_sexp : Sexplib0.Sexp.t -> struct_defn
val sexp_of_struct_defn : struct_defn -> Sexplib0.Sexp.t
type function_defn =
    TFunction of Poppy_parser.Ast_types.Function_name.t *
      Poppy_parser.Ast_types.borrowed_ref option *
      Poppy_parser.Ast_types.type_expr * Poppy_parser.Ast_types.param list *
      block_expr
val function_defn_of_sexp : Sexplib0.Sexp.t -> function_defn
val sexp_of_function_defn : function_defn -> Sexplib0.Sexp.t
type program =
    Prog of struct_defn list * interface_defn list * function_defn list *
      block_expr
val program_of_sexp : Sexplib0.Sexp.t -> program
val sexp_of_program : program -> Sexplib0.Sexp.t
