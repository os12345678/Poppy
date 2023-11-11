module T = Data_race_env.T
module A = Data_race_env.A
module E = Data_race_env
val remove_read_capabilities : E.T.typed_identifier -> E.T.typed_identifier
val type_read_capabilities_expr : T.expr -> T.expr
val type_read_capabilities_block_expr : T.block_expr -> T.block_expr
