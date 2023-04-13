val context : Llvm.llcontext
val builder : Llvm.llbuilder
val the_module : Llvm.llmodule
val named_values : (string, Llvm.llvalue) Hashtbl.t
val counter : int ref
val generate_unique_id : unit -> string
val store_expr_in_global : Llvm.llvalue -> Llvm.llvalue
val string_of_llmodule : Llvm.llmodule -> string
val print_module : Llvm.llmodule -> unit
val codegen_expr : Poppy_parser.Ast.expr -> Llvm.llvalue
val codegen_proto : Poppy_parser.Ast.proto -> Llvm.llvalue
val codegen_block : Poppy_parser.Ast.statement list -> Llvm.llvalue
val codegen_statement : Poppy_parser.Ast.statement -> Llvm.llvalue
