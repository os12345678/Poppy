module T = Desugar_expr.T
module A = Desugar_expr.A
module D = Desugared_ast
module E = Desugar_env
val desugar_program : T.program -> (D.dprogram, Base.Error.t) result
