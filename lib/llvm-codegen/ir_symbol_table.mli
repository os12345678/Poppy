exception Codegen_error of string
module D = Desugar.Desugared_ast
module A = Poppy_parser.Ast_types
module Symbol : sig type t = string val compare : 'a -> 'a -> int end
module SymbolMap :
  sig
    type key = string
    type 'a t = 'a Map.Make(Symbol).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type storage_type = Local | Global
type llvm_symbol_info =
    LVarInfo of { llvm_value : Llvm.llvalue option;
      llvm_type : Llvm.lltype option; storage : storage_type;
      is_global : bool;
    }
  | LFuncInfo of { llvm_function : Llvm.llvalue option;
      params : llvm_symbol_info list;
    }
  | LStructInfo of { llvm_struct : Llvm.lltype option;
      field_map : (string, int) Hashtbl.t;
    }
type llvm_symbol_table = {
  table : llvm_symbol_info SymbolMap.t;
  parent : llvm_symbol_table option;
}
val sym_table_stack : llvm_symbol_table Stack.t
val generate_thread_id : int -> string
val enter_scope : llvm_symbol_table -> llvm_symbol_table
val exit_scope : llvm_symbol_table -> llvm_symbol_table
val add_symbol :
  llvm_symbol_table -> string -> llvm_symbol_info -> llvm_symbol_table
val lookup_struct : llvm_symbol_table -> string -> llvm_symbol_info option
val lookup_variable : llvm_symbol_table -> string -> llvm_symbol_info option
val parse_mangled_method_name : string -> (string * string * string) option
val parse_constructor_call_name : string -> (string * string) option
val print_variable_info_from_symbol : string -> llvm_symbol_info -> unit
val print_variables_from_table : llvm_symbol_table -> unit
val print_all_variables : llvm_symbol_table -> unit
val print_symbol_table : llvm_symbol_table -> unit
val print_symbol_info : llvm_symbol_info -> unit
val process_structs :
  llvm_symbol_table ->
  Desugar.Desugared_ast.dstruct list -> llvm_symbol_table
val process_functions :
  llvm_symbol_table ->
  Desugar.Desugared_ast.dfunction list -> llvm_symbol_table
val process_expr :
  llvm_symbol_table -> Desugar.Desugared_ast.dexpr -> llvm_symbol_table
val process_block :
  llvm_symbol_table -> Desugar.Desugared_ast.dblock -> llvm_symbol_table
val build_symbol_table : Desugar.Desugared_ast.dprogram -> llvm_symbol_table
