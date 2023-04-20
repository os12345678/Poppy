val context : Llvm.llcontext
val builder : Llvm.llbuilder
val the_module : Llvm.llmodule
val scopes : (string, Llvm.llvalue) Hashtbl.t Stack.t
val function_protos :
  (string,
   (string * Poppy_parser.Ast.func_param list * Poppy_parser.Ast.type_decl)
   option)
  Hashtbl.t
val counter : int ref
val generate_unique_id : unit -> string
val string_of_llmodule : Llvm.llmodule -> string
val enter_scope : unit -> unit
val exit_scope : unit -> unit
val current_scope : unit -> (string, Llvm.llvalue) Hashtbl.t
val add_var_to_current_scope : string -> Llvm.llvalue -> Llvm.llvalue
val lookup_var_in_scopes : string -> Llvm.llvalue option
val find_function_prototype :
  string ->
  (string * Poppy_parser.Ast.func_param list * Poppy_parser.Ast.type_decl)
  option option
val is_valid_main_function_signature :
  'a list -> Poppy_parser.Ast.type_decl -> bool
val is_void : Llvm.lltype -> bool
val llvm_type_of_ast_type : Poppy_parser.Ast.typ -> Llvm.lltype
val codegen_expr : Poppy_parser.Ast.expr -> Llvm.llvalue
val codegen_block :
  Poppy_parser.Ast.statement list -> Poppy_parser.Ast.statement option
val codegen_statement : Poppy_parser.Ast.statement -> Llvm.llvalue option
val codegen_ast : Poppy_parser.Ast.statement list -> Llvm.llmodule
val codegen_ast_to_string : Poppy_parser.Ast.statement list -> string
