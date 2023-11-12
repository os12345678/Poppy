module T = Desugar_env.TAst
module Typ = Desugar_env.T
module D = Desugared_ast
module E = Desugar_env
val desugar_function_defn : T.function_defn -> D.dfunction
