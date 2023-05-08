exception Codegen_error of string
val context : Llvm.llcontext
val builder : Llvm.llbuilder
val the_module : Llvm.llmodule
val named_values : (string, Llvm.llvalue) Hashtbl.t
val function_protos :
  (string,
   (string * Poppy_parser.Ast.func_param list * Poppy_parser.Ast.type_decl)
   option)
  Hashtbl.t
val class_protos : (string, Scoping.class_info) Hashtbl.t
val codegen_expr :
  Poppy_parser.Ast.expr ->
  Scoping.scope ->
  Scoping.class_info ->
  Llvm.llbuilder -> (string, Llvm.llvalue) Hashtbl.t -> Llvm.llvalue
