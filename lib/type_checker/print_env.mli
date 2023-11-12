val struct_defn_to_string : Poppy_parser.Ast.struct_defn -> string
val trait_defn_to_string : Poppy_parser.Ast.trait_defn -> string
val method_defn_to_string : Poppy_parser.Ast.method_defn -> string
val function_defn_to_string : Poppy_parser.Ast.function_defn -> string
val print_env : Type_env.env -> unit
val print_block_scope : Type_env.env -> unit
