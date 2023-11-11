val check_no_duplicate_trait_names :
  Poppy_parser.Ast.trait_defn list -> unit Core.Or_error.t
val check_no_duplicate_method_signatures :
  Poppy_parser.Ast.trait_defn list -> unit Core.Or_error.t
val type_trait_defn :
  Poppy_parser.Ast.trait_defn -> (Typed_ast.trait_defn, Core.Error.t) result
val type_trait_defns :
  Poppy_parser.Ast.trait_defn list ->
  (Typed_ast.trait_defn list, Core.Error.t) result
