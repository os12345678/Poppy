open Poppy_parser.Ast
exception DuplicateClass of string
exception DuplicateMemberVariable of string
exception DuplicateMemberMethod of string
exception DuplicateLocalVariable of string

type value =
  | Variable of expr
  | Parameter of expr
  | ReturnValue of expr
  | ClassInstance of class_info * (string, value) Hashtbl.t


and class_info = {
  parent_class: class_info option;
  class_scope: scope;
  member_variables: (string, value) Hashtbl.t;
  member_methods: (string, (statement * string list)) Hashtbl.t;
}

and scope = {
  parent: scope option; (* optional field that refers to the parent scope in the hierarchy. *)
  table: (string, value) Hashtbl.t; (* hash table that maps string keys (identifiers) to values, such as variables and method parameters. *)
  class_table: (string, class_info) Hashtbl.t; (* hash table that maps string keys (class names) to class_info values, storing information about the classes in the program. *)
}

let create_instance (class_info : class_info) : value =
  let instance_data = Hashtbl.copy class_info.member_variables in
  ClassInstance (class_info, instance_data)

let create_new_scope parent =
  {
    parent;
    table = Hashtbl.create 10;
    class_table = Hashtbl.create 10;
  }

let add_class (current_scope : scope) (class_name : string) (class_info : class_info) =
  if Hashtbl.mem current_scope.class_table class_name then
    raise (DuplicateClass class_name)
  else
    Hashtbl.add current_scope.class_table class_name class_info
  
let add_member_variable (class_info : class_info) (var_name : string) (value : value) =
  if Hashtbl.mem class_info.member_variables var_name then
    raise (DuplicateMemberVariable var_name)
  else
    Hashtbl.add class_info.member_variables var_name value
  
let add_member_method (class_info : class_info) (method_name : string) (stmt : statement) (arg_names : string list) =
  if Hashtbl.mem class_info.member_methods method_name then
    raise (DuplicateMemberMethod method_name)
  else
    Hashtbl.add class_info.member_methods method_name (stmt, arg_names)
      
let add_local_variable (current_scope : scope) (var_name : string) (value : value) =
  if Hashtbl.mem current_scope.table var_name then
    raise (DuplicateLocalVariable var_name)
  else
    Hashtbl.add current_scope.table var_name value

(* Finds an identifier by looking up in the current scope and its ancestors *)
let rec find_identifier (current_scope : scope) (id_name : string) : value option =
  try
    Some (Hashtbl.find current_scope.table id_name)
  with Not_found ->
    match current_scope.parent with
    | Some parent_scope -> find_identifier parent_scope id_name
    | None -> None

