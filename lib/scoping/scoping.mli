exception DuplicateClass of string
exception DuplicateMemberVariable of string
exception DuplicateMemberMethod of string
exception DuplicateLocalVariable of string
type value =
    Variable of Poppy_parser.Ast.expr
  | Parameter of Poppy_parser.Ast.expr
  | ReturnValue of Poppy_parser.Ast.expr
  | ClassInstance of class_info * (string, value) Hashtbl.t
and class_info = {
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
val add_class : scope -> string -> class_info -> unit
val add_member_variable : class_info -> string -> value -> unit
val add_member_method :
  class_info -> string -> Poppy_parser.Ast.statement -> string list -> unit
val add_local_variable : scope -> string -> value -> unit
val find_identifier : scope -> string -> value option
