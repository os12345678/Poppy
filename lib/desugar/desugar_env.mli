module TAst = Poppy_type_checker.Typed_ast
module T = Poppy_parser.Ast_types
val mangle_name : T.Method_name.t -> string
val mangle_impl :
  T.Trait_name.t -> T.Struct_name.t -> T.Method_name.t -> string
val mangle_mapp :
  T.Trait_name.t -> T.Struct_name.t -> T.Method_name.t -> string
val desugar_param : T.param -> T.type_expr * string
