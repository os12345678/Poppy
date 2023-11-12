type identifier =
    Variable of Ast_types.Var_name.t
  | ObjField of Ast_types.Var_name.t * Ast_types.Field_name.t
val identifier_of_sexp : Sexplib0.Sexp.t -> identifier
val sexp_of_identifier : identifier -> Sexplib0.Sexp.t
type expr = { loc : Ast_types.loc; node : expr_node; }
and expr_node =
    Int of int
  | Boolean of bool
  | Identifier of identifier
  | Let of Ast_types.type_expr option * Ast_types.Var_name.t * expr
  | Assign of identifier * expr
  | Consume of identifier
  | Constructor of Ast_types.Var_name.t * Ast_types.Struct_name.t *
      constructor_arg list
  | MethodApp of Ast_types.Var_name.t * Ast_types.Method_name.t * expr list
  | FunctionApp of Ast_types.Function_name.t * expr list
  | If of expr * block_expr * block_expr
  | While of expr * block_expr
  | For of expr * expr * expr * block_expr
  | Printf of string * expr list
  | BinOp of Ast_types.bin_op * expr * expr
  | UnOp of Ast_types.un_op * expr
  | FinishAsync of async_expr list * block_expr
and block_expr = Block of Ast_types.loc * expr list
and async_expr = AsyncExpr of block_expr
and constructor_arg = ConstructorArg of Ast_types.Field_name.t * expr
val expr_of_sexp : Sexplib0.Sexp.t -> expr
val expr_node_of_sexp : Sexplib0.Sexp.t -> expr_node
val block_expr_of_sexp : Sexplib0.Sexp.t -> block_expr
val async_expr_of_sexp : Sexplib0.Sexp.t -> async_expr
val constructor_arg_of_sexp : Sexplib0.Sexp.t -> constructor_arg
val sexp_of_expr : expr -> Sexplib0.Sexp.t
val sexp_of_expr_node : expr_node -> Sexplib0.Sexp.t
val sexp_of_block_expr : block_expr -> Sexplib0.Sexp.t
val sexp_of_async_expr : async_expr -> Sexplib0.Sexp.t
val sexp_of_constructor_arg : constructor_arg -> Sexplib0.Sexp.t
type struct_defn =
    TStruct of Ast_types.Struct_name.t * Ast_types.capability list *
      Ast_types.field_defn list
val struct_defn_of_sexp : Sexplib0.Sexp.t -> struct_defn
val sexp_of_struct_defn : struct_defn -> Sexplib0.Sexp.t
type method_defn = TMethod of Ast_types.method_signature * block_expr
val method_defn_of_sexp : Sexplib0.Sexp.t -> method_defn
val sexp_of_method_defn : method_defn -> Sexplib0.Sexp.t
type impl_defn =
    TImpl of Ast_types.Trait_name.t * Ast_types.Struct_name.t *
      method_defn list
val impl_defn_of_sexp : Sexplib0.Sexp.t -> impl_defn
val sexp_of_impl_defn : impl_defn -> Sexplib0.Sexp.t
type trait_defn =
    TTrait of Ast_types.Trait_name.t * Ast_types.method_signature list
val trait_defn_of_sexp : Sexplib0.Sexp.t -> trait_defn
val sexp_of_trait_defn : trait_defn -> Sexplib0.Sexp.t
type function_defn = TFunction of Ast_types.function_signature * block_expr
val function_defn_of_sexp : Sexplib0.Sexp.t -> function_defn
val sexp_of_function_defn : function_defn -> Sexplib0.Sexp.t
type program =
    Prog of struct_defn list * trait_defn list * impl_defn list *
      function_defn list * block_expr
val program_of_sexp : Sexplib0.Sexp.t -> program
val sexp_of_program : program -> Sexplib0.Sexp.t
