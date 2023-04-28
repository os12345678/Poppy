open Poppy_parser.Ast
exception DuplicateClass of string
exception DuplicateMemberVariable of string
exception DuplicateMemberMethod of string
exception DuplicateLocalVariable of string

type value =
  | Variable of expr * typ
  | Parameter of expr * typ
  | ReturnValue of expr * typ 
  | ClassInstance of class_info * (string, value) Hashtbl.t

  and class_info = {
    class_name: string;
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

let create_new_class_info class_name parent_class =
  {
    class_name;
    parent_class;
    class_scope = create_new_scope None;
    member_variables = Hashtbl.create 10;
    member_methods = Hashtbl.create 10;
  }

let add_class (current_scope : scope) (class_name : string) (parent_class : class_info option) =
  let class_info = create_new_class_info class_name parent_class in
  if Hashtbl.mem current_scope.class_table class_name then
    raise (DuplicateClass class_name)
  else
    Hashtbl.add current_scope.class_table class_name class_info
  
let add_identifier (current_scope : scope) (var_name : string) (typ : typ) =
  if Hashtbl.mem current_scope.table var_name then
    raise (DuplicateLocalVariable var_name)
  else
    Hashtbl.add current_scope.table var_name (Variable (Id var_name, typ))
  
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
let rec find_identifier (current_scope : scope) (id_name : string) : typ option =
  (* print current scope and id name *)
  print_endline (Printf.sprintf "current scope: %s" (Hashtbl.fold (fun k _v acc -> acc ^ k ^ ", ") current_scope.table ""));
  print_endline (Printf.sprintf "id name: %s" id_name);
  try
    match Hashtbl.find current_scope.table id_name with
    | Variable (_, typ) -> Some typ
    | Parameter (_, typ) -> Some typ
    | ReturnValue (_, typ) -> Some typ
    | ClassInstance (class_info, _) -> Some (ClassInstance class_info.class_name)
  with Not_found ->
    match current_scope.parent with
    | Some parent_scope -> find_identifier parent_scope id_name
    | None -> None

let rec find_expr_type (expr : expr)  : typ =
  match expr with
  | IntLiteral _ -> Int
  | BoolLiteral _ -> Bool
  | StringLiteral _ -> String
  | Id _ -> failwith "Cannot determine the type of an identifier without scope information"
  | BinOp (_, left, _) -> find_expr_type left (* Assumes the binary operation returns the same type as its operands *)
  | Call (callee, _) -> failwith (Printf.sprintf "Cannot determine the return type of function %s without scope information" callee)
  | ClassInstantiation (_, _) -> failwith "Cannot determine the type of a class instantiation without scope information"
  | ClassMemberAccess (_, _) -> failwith "Cannot determine the type of a class member access without scope information"
  (* | This -> (
    match current_class_info with
    | Some class_info -> ClassInstance class_info.class_name
    | None -> failwith "Cannot determine the type of 'this' outside of a class context"
  ) *)
| _ -> Void
 

let rec find_return_type (stmt : statement) : typ =
  match stmt with
  | Return expr -> find_expr_type expr
  | If (_, stmt1, _) -> find_return_type stmt1
  | While (_, stmt) -> find_return_type stmt
  | Block stmts -> find_return_type (List.hd (List.rev stmts))
  | _ -> Void

let rec find_method_return_type (class_info : class_info) (method_name : string) : typ option =
  try
    match Hashtbl.find class_info.member_methods method_name with
    | stmt, _ -> Some (find_return_type stmt)
  with Not_found ->
    match class_info.parent_class with
    | Some parent_class_info -> find_method_return_type parent_class_info method_name
    | None -> None
    
let rec find_class (current_scope : scope) (class_name : string) : class_info option =
  try
    Some (Hashtbl.find current_scope.class_table class_name)
  with Not_found ->
    match current_scope.parent with
    | Some parent_scope -> find_class parent_scope class_name
    | None -> None

let rec find_member_variable (class_info : class_info) (var_name : string) : value option =
  try
    Some (Hashtbl.find class_info.member_variables var_name)
  with Not_found ->
    match class_info.parent_class with
    | Some parent_class_info -> find_member_variable parent_class_info var_name
    | None -> None
    

