module D = Desugar.Desugared_ast
module T = Poppy_parser.Ast_types
module St = Ir_symbol_table
val context : Llvm.llcontext
val the_module : Llvm.llmodule
val builder : Llvm.llbuilder
val active_threads_table : (string, Llvm.llvalue list) Hashtbl.t
val wrap : D.T.loc -> D.T.type_expr -> D.dexpr_node -> D.dexpr
val llvm_type_of_typ : T.type_expr -> Llvm.lltype
val handle_string_constant : Llvm.llvalue -> Llvm.llvalue
val is_valid_thread_id : string -> Llvm.llvalue list
val clear_thread_id : string -> unit
val check_and_remove_thread_from_table : string -> Llvm.llvalue option
val add_thread_to_table : string -> Llvm.llvalue -> unit
val print_hash_table : unit -> unit
val generate_wrapper_function :
  string -> Llvm.lltype list -> Llvm.lltype -> Llvm.llvalue
val pack_args : Llvm.llvalue list -> Llvm.lltype list -> Llvm.llvalue
val adjust_arg_type : Llvm.lltype -> Llvm.llvalue -> Llvm.llvalue
val is_string_literal : Llvm.llvalue -> bool
val define_global_string :
  string -> Llvm.llmodule -> Llvm.llcontext -> Llvm.llvalue
