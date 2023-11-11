module T = Desugar_env.TAst
module Typ = Desugar_env.T
module D = Desugared_ast
val desugar_impl : T.impl_defn list -> D.dfunction list
val desugar_impl_single : T.impl_defn -> D.dfunction list
val desugar_method :
  Desugar_env.T.Struct_name.t ->
  Desugar_env.T.Trait_name.t -> T.method_defn -> D.dfunction
