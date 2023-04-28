exception DuplicateClass of string
exception DuplicateMemberVariable of string
exception DuplicateMemberMethod of string
exception DuplicateLocalVariable of string
type value =
    Variable of Poppy_parser.Ast.expr * Poppy_parser.Ast.typ
  | Parameter of Poppy_parser.Ast.expr * Poppy_parser.Ast.typ
  | ReturnValue of Poppy_parser.Ast.expr * Poppy_parser.Ast.typ
  | ClassInstance of class_info * (string, value) Hashtbl.t
and class_info = {
  class_name : string;
  parent_class : class_info option;
  class_scope : scope;
  member_variables : (string, value) Hashtbl.t;
  member_methods :
    (string, Poppy_parser.Ast.statement * string list) Hashtbl.t;
}
and scope = {
  parent : scope option;
  table : (string, value) Hashtbl.t;
  class_table : (string, class_info) Hashtbl.t;
}
val create_instance : class_info -> value
val create_new_scope : scope option -> scope
val create_new_class_info : string -> class_info option -> class_info
val add_class : scope -> string -> class_info option -> unit
val add_identifier : scope -> string -> Poppy_parser.Ast.typ -> unit
val add_member_variable : class_info -> string -> value -> unit
val add_member_method :
  class_info -> string -> Poppy_parser.Ast.statement -> string list -> unit
val add_local_variable : scope -> string -> value -> unit
val find_identifier : scope -> string -> Poppy_parser.Ast.typ option
val find_expr_type : Poppy_parser.Ast.expr -> Poppy_parser.Ast.typ
val find_return_type : Poppy_parser.Ast.statement -> Poppy_parser.Ast.typ
val find_method_return_type :
  class_info -> string -> Poppy_parser.Ast.typ option
val find_class : scope -> string -> class_info option
val find_member_variable : class_info -> string -> value option
