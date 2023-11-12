exception Codegen_error of string
module L = Llvm
module D = Codegen_util.D
module U = Codegen_util
module T = Codegen_util.T
module St = Ir_symbol_table
module E = Link_extern
val generate_thread_id : int -> string
val codegen_expr :
  D.dexpr ->
  St.llvm_symbol_table ->
  [ `Function ] L.PassManager.t -> St.llvm_symbol_table * L.llvalue
val codegen_block :
  D.dblock ->
  St.llvm_symbol_table ->
  [ `Function ] L.PassManager.t -> St.llvm_symbol_table * Llvm.llvalue
val codegen_proto :
  D.dfunction -> St.llvm_symbol_table -> St.llvm_symbol_table * Llvm.llvalue
val codegen_func_body :
  D.dfunction ->
  Llvm.llvalue ->
  St.llvm_symbol_table ->
  [ `Function ] L.PassManager.t -> St.llvm_symbol_table
val codegen_struct_init_function : D.dstruct -> L.lltype -> L.llvalue
val codegen_structs :
  D.dstruct list -> St.llvm_symbol_table -> St.llvm_symbol_table
val codegen_main :
  D.dblock -> St.llvm_symbol_table -> [ `Function ] L.PassManager.t -> unit
val codegen_ast :
  D.dprogram ->
  St.llvm_symbol_table -> [ `Function ] L.PassManager.t -> Llvm.llmodule
