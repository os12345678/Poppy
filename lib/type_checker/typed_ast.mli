type obj_var_and_capabilities =
    Poppy_parser.Ast_types.Var_name.t *
    Poppy_parser.Ast_types.Struct_name.t *
    Poppy_parser.Ast_types.capability list
val obj_var_and_capabilities_of_sexp :
  Sexplib0.Sexp.t -> obj_var_and_capabilities
val sexp_of_obj_var_and_capabilities :
  obj_var_and_capabilities -> Sexplib0.Sexp.t
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
  | TBlockExpr of block_expr
  | TAssign of typed_identifier * expr
  | TConstructor of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.Struct_name.t * constructor_arg list
  | TConsume of typed_identifier
  | TMethodApp of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.Struct_name.t *
      Poppy_parser.Ast_types.Trait_name.t *
      Poppy_parser.Ast_types.Method_name.t *
      Poppy_parser.Ast_types.capability list * expr list
  | TFunctionApp of Poppy_parser.Ast_types.Function_name.t * expr list
  | TIf of expr * block_expr * block_expr
  | TWhile of expr * block_expr
  | TPrintf of string * expr list
  | TBinOp of Poppy_parser.Ast_types.bin_op * expr * expr
  | TUnOp of Poppy_parser.Ast_types.un_op * expr
  | TFinishAsync of async_expr list * obj_var_and_capabilities list *
      block_expr
and typed_identifier =
    TVariable of Poppy_parser.Ast_types.Var_name.t *
      Poppy_parser.Ast_types.type_expr *
      Poppy_parser.Ast_types.capability list *
      Poppy_parser.Ast_types.borrowed_ref option
  | TObjField of Poppy_parser.Ast_types.Struct_name.t *
      Poppy_parser.Ast_types.Var_name.t * Poppy_parser.Ast_types.type_expr *
      Poppy_parser.Ast_types.Field_name.t *
      Poppy_parser.Ast_types.capability list *
      Poppy_parser.Ast_types.borrowed_ref option
and block_expr =
    Block of Poppy_parser.Ast_types.loc * Poppy_parser.Ast_types.type_expr *
      expr list
and async_expr = AsyncExpr of obj_var_and_capabilities list * block_expr
and constructor_arg =
    ConstructorArg of Poppy_parser.Ast_types.Field_name.t * expr
val expr_of_sexp : Sexplib0.Sexp.t -> expr
val expr_node_of_sexp : Sexplib0.Sexp.t -> expr_node
val typed_identifier_of_sexp : Sexplib0.Sexp.t -> typed_identifier
val block_expr_of_sexp : Sexplib0.Sexp.t -> block_expr
val async_expr_of_sexp : Sexplib0.Sexp.t -> async_expr
val constructor_arg_of_sexp : Sexplib0.Sexp.t -> constructor_arg
val sexp_of_expr : expr -> Sexplib0.Sexp.t
val sexp_of_expr_node : expr_node -> Sexplib0.Sexp.t
val sexp_of_typed_identifier : typed_identifier -> Sexplib0.Sexp.t
val sexp_of_block_expr : block_expr -> Sexplib0.Sexp.t
val sexp_of_async_expr : async_expr -> Sexplib0.Sexp.t
val sexp_of_constructor_arg : constructor_arg -> Sexplib0.Sexp.t
type struct_defn =
    TStruct of Poppy_parser.Ast_types.Struct_name.t *
      Poppy_parser.Ast_types.capability list *
      Poppy_parser.Ast_types.field_defn list
val struct_defn_of_sexp : Sexplib0.Sexp.t -> struct_defn
val sexp_of_struct_defn : struct_defn -> Sexplib0.Sexp.t
type method_defn =
    TMethod of Poppy_parser.Ast_types.method_signature * block_expr
val method_defn_of_sexp : Sexplib0.Sexp.t -> method_defn
val sexp_of_method_defn : method_defn -> Sexplib0.Sexp.t
type impl_defn =
    TImpl of Poppy_parser.Ast_types.Trait_name.t *
      Poppy_parser.Ast_types.Struct_name.t * method_defn list
val impl_defn_of_sexp : Sexplib0.Sexp.t -> impl_defn
val sexp_of_impl_defn : impl_defn -> Sexplib0.Sexp.t
type trait_defn =
    TTrait of Poppy_parser.Ast_types.Trait_name.t *
      Poppy_parser.Ast_types.method_signature list
val trait_defn_of_sexp : Sexplib0.Sexp.t -> trait_defn
val sexp_of_trait_defn : trait_defn -> Sexplib0.Sexp.t
type function_defn =
    TFunction of Poppy_parser.Ast_types.function_signature * block_expr
val function_defn_of_sexp : Sexplib0.Sexp.t -> function_defn
val sexp_of_function_defn : function_defn -> Sexplib0.Sexp.t
type program =
    Prog of struct_defn list * trait_defn list * impl_defn list *
      function_defn list * block_expr
val program_of_sexp : Sexplib0.Sexp.t -> program
val sexp_of_program : program -> Sexplib0.Sexp.t
