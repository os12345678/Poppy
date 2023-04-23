type context = {
  variables : (string, Poppy_parser.Ast.typ) Core.Map.Poly.t;
  functions :
    (string, Poppy_parser.Ast.typ list * Poppy_parser.Ast.typ)
    Core.Map.Poly.t;
}
val empty_context : context
val check_expr : context -> Poppy_parser.Ast.expr -> Poppy_parser.Ast.typ
val check_statement : context -> Poppy_parser.Ast.statement -> context
val check_program : Poppy_parser.Ast.statement list -> unit
