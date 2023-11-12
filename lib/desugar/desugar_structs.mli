module T = Poppy_type_checker.Typed_ast
module Typ = Desugared_ast.T
module D = Desugared_ast
val desugar_struct_defn : T.struct_defn -> D.dstruct
val desugar_field_defn : Desugared_ast.T.field_defn -> string * D.T.type_expr
