module T = Desugar_env.TAst
module A = Desugar_env.T
module E = Desugar_env
val generate_thread_id : int -> string
val extract_var_name : T.typed_identifier -> string
type function_call = { fname : string; args : Desugared_ast.dexpr list; }
val thread_counters : (string, int) Base.Hashtbl.t
val get_next_thread_name : string -> string
val desugar_expr : T.expr -> Desugared_ast.dexpr
val desugar_block : T.block_expr -> Desugared_ast.dblock
val desugar_async_exprs : T.async_expr -> Desugared_ast.dexpr list
