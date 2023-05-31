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
  | MethodApp of Ast_types.Var_name.t * Ast_types.Method_name.t * expr list
  | FunctionApp of Ast_types.Function_name.t * expr list
  | FinishAsync of Ast_types.loc * async_expr list * block_expr
  | If of expr * block_expr * block_expr
  | While of expr * block_expr
  | For of expr * expr * expr * block_expr
  | BinOp of Ast_types.bin_op * expr * expr
  | UnOp of Ast_types.un_op * expr
  | NewStruct of Ast_types.Struct_name.t *
      (Ast_types.Field_name.t * expr) list
  | AssignToInterface of Ast_types.Var_name.t * expr
and block_expr = Block of Ast_types.loc * expr list
and async_expr = AsyncExpr of block_expr
val expr_of_sexp : Sexplib0.Sexp.t -> expr
val expr_node_of_sexp : Sexplib0.Sexp.t -> expr_node
val block_expr_of_sexp : Sexplib0.Sexp.t -> block_expr
val async_expr_of_sexp : Sexplib0.Sexp.t -> async_expr
val sexp_of_expr : expr -> Sexplib0.Sexp.t
val sexp_of_expr_node : expr_node -> Sexplib0.Sexp.t
val sexp_of_block_expr : block_expr -> Sexplib0.Sexp.t
val sexp_of_async_expr : async_expr -> Sexplib0.Sexp.t
type method_defn =
    TMethod of Ast_types.Method_name.t * Ast_types.borrowed_ref option *
      Ast_types.type_expr * Ast_types.param list * block_expr
val method_defn_of_sexp : Sexplib0.Sexp.t -> method_defn
val sexp_of_method_defn : method_defn -> Sexplib0.Sexp.t
type struct_defn =
    TStruct of Ast_types.Struct_name.t * Ast_types.capability list *
      Ast_types.field_defn list * method_defn list
val struct_defn_of_sexp : Sexplib0.Sexp.t -> struct_defn
val sexp_of_struct_defn : struct_defn -> Sexplib0.Sexp.t
type interface_method_defn =
    TInterfaceMethod of Ast_types.Method_name.t *
      Ast_types.borrowed_ref option * Ast_types.type_expr *
      Ast_types.param list
val interface_method_defn_of_sexp : Sexplib0.Sexp.t -> interface_method_defn
val sexp_of_interface_method_defn : interface_method_defn -> Sexplib0.Sexp.t
type interface_defn =
    TInterface of Ast_types.Interface_name.t * interface_method_defn list
val interface_defn_of_sexp : Sexplib0.Sexp.t -> interface_defn
val sexp_of_interface_defn : interface_defn -> Sexplib0.Sexp.t
type function_defn =
    TFunction of Ast_types.Function_name.t * Ast_types.borrowed_ref option *
      Ast_types.type_expr * Ast_types.param list * block_expr
val function_defn_of_sexp : Sexplib0.Sexp.t -> function_defn
val sexp_of_function_defn : function_defn -> Sexplib0.Sexp.t
type program =
    Prog of struct_defn list * interface_defn list * function_defn list *
      block_expr
val program_of_sexp : Sexplib0.Sexp.t -> program
val sexp_of_program : program -> Sexplib0.Sexp.t
