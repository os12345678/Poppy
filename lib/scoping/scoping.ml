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
    member_variables: (string, (access_modifier * value)) Hashtbl.t;
    member_methods: (string, (access_modifier * (statement * string list))) Hashtbl.t;
  }
  
and scope = {
  parent: scope option; (* optional field that refers to the parent scope in the hierarchy. *)
  table: (string, value) Hashtbl.t; (* hash table that maps string keys (identifiers) to values, such as variables and method parameters. *)
  class_table: (string, class_info) Hashtbl.t; (* hash table that maps string keys (class names) to class_info values, storing information about the classes in the program. *)
}

let create_instance (class_info : class_info) : value =
  let instance_data = Hashtbl.create (Hashtbl.length class_info.member_variables) in
  Hashtbl.iter (fun key (_access, value) -> Hashtbl.add instance_data key value) class_info.member_variables;
  ClassInstance (class_info, instance_data)

let create_new_scope parent =
  {
    parent;
    table = Hashtbl.create 10;
    class_table = Hashtbl.create 10;
  }

let scope_level (scope : scope) : int =
  let rec scope_level_aux (scope : scope) (level : int) : int =
    match scope.parent with
    | Some parent_scope -> scope_level_aux parent_scope (level + 1)
    | None -> level
  in
  scope_level_aux scope 0

let create_new_class_info class_name parent_class =
  {
    class_name;
    parent_class;
    class_scope = create_new_scope None;
    member_variables = Hashtbl.create 10;
    member_methods = Hashtbl.create 10;
  }

  let add_class (current_scope : scope) (class_name : string) (class_info : class_info) : unit =
    print_endline (Printf.sprintf "Adding class %s to scope level %d" class_name (scope_level current_scope));
    if Hashtbl.mem current_scope.class_table class_name then
      raise (Failure (Printf.sprintf "Duplicate class declaration: %s" class_name))
    else
      Hashtbl.add current_scope.class_table class_name class_info
    
let add_identifier (current_scope : scope) (var_name : string) (typ : typ) =
  if Hashtbl.mem current_scope.table var_name then
    raise (DuplicateLocalVariable var_name)
  else
    Hashtbl.add current_scope.table var_name (Variable (Id var_name, typ))
  
let add_member_variable (class_info : class_info) (var_name : string) (access_and_value : access_modifier * value) =
  if Hashtbl.mem class_info.member_variables var_name then
    raise (DuplicateMemberVariable var_name)
  else
    Hashtbl.add class_info.member_variables var_name access_and_value
  
let add_member_method (class_info : class_info) (method_name : string) (access_and_method_data : access_modifier * (statement * string list)) =
  if Hashtbl.mem class_info.member_methods method_name then
    raise (DuplicateMemberMethod method_name)
  else
    Hashtbl.add class_info.member_methods method_name access_and_method_data
      
let add_local_variable (current_scope : scope) (var_name : string) (value : value) =
  if Hashtbl.mem current_scope.table var_name then
    raise (DuplicateLocalVariable var_name)
  else
    Hashtbl.add current_scope.table var_name value

(* Finds an identifier by looking up in the current scope and its ancestors *)
let rec find_identifier (current_scope : scope) (id_name : string) : typ option =
  (* print_endline (Printf.sprintf "current scope: %s" (Hashtbl.fold (fun k _v acc -> acc ^ k ^ ", ") current_scope.table ""));
  print_endline (Printf.sprintf "id name: %s" id_name); *)
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
  | ClassInstantiation (_, _, _) -> failwith "Cannot determine the type of a class instantiation without scope information"
  | ClassMemberAccess (_, _) -> failwith "Cannot determine the type of a class member access without scope information"
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
    | (_access, (stmt, _)) -> Some (find_return_type stmt)
  with Not_found ->
    match class_info.parent_class with
    | Some parent_class_info -> find_method_return_type parent_class_info method_name
    | None -> None
    
let find_class (current_scope : scope) (class_name : string) : class_info option =
  let rec find_class_aux (scope : scope) : class_info option =
    print_endline (Printf.sprintf "Searching for class %s in scope level %d" class_name (scope_level scope));
    match Hashtbl.find_opt scope.class_table class_name with
    | Some class_info ->
      print_endline (Printf.sprintf "Found class %s" class_name); 
      Some class_info
    | None ->
      match scope.parent with
      | Some parent_scope -> find_class_aux parent_scope
      | None -> None
  in
  find_class_aux current_scope
  
let rec get_global_scope (current_scope : scope) : scope =
  match current_scope.parent with
  | Some parent_scope -> get_global_scope parent_scope
  | None -> current_scope
    
let rec find_member_variable (class_info : class_info) (var_name : string) : (access_modifier * value) option =
  try
    Some (Hashtbl.find class_info.member_variables var_name)
  with Not_found ->
    match class_info.parent_class with
    | Some parent_class_info -> find_member_variable parent_class_info var_name
    | None -> None
    
let rec find_member_variable_in_class_and_ancestors (class_info : class_info) (member_name : string) : (access_modifier * value) option =
  match find_member_variable class_info member_name with
  | Some member -> Some member
  | None ->
    match class_info.parent_class with
    | Some parent_class_info -> find_member_variable_in_class_and_ancestors parent_class_info member_name
    | None -> None

  let string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Void -> "void"
  | ClassInstance class_name -> "ClassInstance " ^ class_name
  | _ -> "Unknown"
  
let string_of_value = function
  | Variable (_, typ) -> "Variable (" ^ (string_of_typ typ) ^ ")"
  | Parameter (_, typ) -> "Parameter (" ^ (string_of_typ typ) ^ ")"
  | ReturnValue (_, typ) -> "ReturnValue (" ^ (string_of_typ typ) ^ ")"
  | ClassInstance (class_info, _) -> "ClassInstance " ^ class_info.class_name


  let rec string_of_expr expr =
    match expr with
    | IntLiteral i -> "IntLiteral (" ^ (string_of_int i) ^ ")"
    | BoolLiteral b -> "BoolLiteral (" ^ (string_of_bool b) ^ ")"
    | StringLiteral s -> "StringLiteral (" ^ s ^ ")"
    | Id id -> "Id (" ^ id ^ ")"
    | BinOp (op, left, right) -> "BinOp (" ^ (string_of_binop op) ^ ", " ^ (string_of_expr left) ^ ", " ^ (string_of_expr right) ^ ")"
    | Call (callee, args) -> "Call (" ^ callee ^ ", " ^ (String.concat ", " (List.map string_of_expr args)) ^ ")"
    | ClassInstantiation (class_name, _constructor, args) -> "ClassInstantiation (" ^ class_name ^ ", " ^ (String.concat ", " (List.map string_of_expr args)) ^ ")"
    | ClassMemberAccess (expr, member_name) -> "ClassMemberAccess (" ^ (string_of_expr expr) ^ ", " ^ member_name ^ ")"
    | _ -> "Unknown expression"
  
  and string_of_binop = function
    | Plus -> "Add"
    | Minus -> "Sub"
    | Times -> "Mult"
    | Div -> "Div"
    | And -> "And"
    | Or -> "Or"
    | Eq -> "Eq"
    | Neq -> "Neq"
    | Lt -> "Lt"
    | Leq -> "Leq"
    | Gt -> "Gt"
    | Geq -> "Geq"
    | Xor -> "Xor"
  
  let string_of_access_modifier = function
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

  let  string_of_statement stmt =
    match stmt with
    | Return expr -> "Return (" ^ (string_of_expr expr) ^ ")"
    | If (expr, _stmt1, _stmt2) -> "If (" ^ (string_of_expr expr) ^ ")"
    | While (expr, _stmt) -> "While (" ^ (string_of_expr expr) ^ ")"
    | Block _ -> "Block"
    | _ -> "Unknown statement"
  
let rec print_scope_contents (current_scope : scope) (indent_level : int) : unit =
  let indent = String.make (indent_level * 2) ' ' in

  (match current_scope.parent with
  | Some parent_scope ->
    print_scope_contents parent_scope (indent_level + 1)
  | None -> ());

  print_endline (Printf.sprintf "%s=== Identifiers in the current scope (level %d) ===" indent indent_level);
  Hashtbl.iter (fun key value ->
    print_endline (Printf.sprintf "%s%s: %s" indent key (string_of_value value))
  ) current_scope.table;

  print_endline (Printf.sprintf "%s=== Classes in the current scope (level %d) ===" indent indent_level);
  Hashtbl.iter (fun key class_info ->
    print_endline (Printf.sprintf "%s%s: %s" indent key class_info.class_name);

    print_endline (Printf.sprintf "%s  === Member Variables ===" indent);
    Hashtbl.iter (fun key (access_modifier, value) ->
      print_endline (Printf.sprintf "%s  %s (%s): %s" indent key (string_of_access_modifier access_modifier) (string_of_value value))
    ) class_info.member_variables;

    print_endline (Printf.sprintf "%s  === Member Methods ===" indent);
    Hashtbl.iter (fun key (access_modifier, (stmt, _param_names)) ->
      print_endline (Printf.sprintf "%s  %s (%s): %s" indent key (string_of_access_modifier access_modifier) (string_of_statement stmt))
    ) class_info.member_methods;

  ) current_scope.class_table;
  
  print_endline (Printf.sprintf "%s======================================" indent);
  

