val context : Llvm.llcontext
val builder : Llvm.llbuilder
val the_module : Llvm.llmodule
val llvm_type_of_typ : Llvm.llcontext -> Poppy_parser.Ast.typ -> Llvm.lltype
val func_map : (string, Llvm.llvalue) Hashtbl.t
val is_pointer : Llvm.lltype -> bool
val find_function :
  (string, Llvm.llvalue) Hashtbl.t -> string -> Llvm.llvalue option
val find_named_value :
  string -> (string, Llvm.llvalue) Hashtbl.t -> Llvm.llvalue option
val codegen_binop :
  Poppy_parser.Ast.bin_op -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
val codegen_class_instantiation :
  Scoping.scope ->
  string ->
  string ->
  Poppy_parser.Ast.expr list ->
  (Scoping.class_info -> Llvm.llvalue) -> Llvm.llvalue
val codegen_call :
  string ->
  Llvm.llvalue list -> (string, Llvm.llvalue) Hashtbl.t -> Llvm.llvalue
