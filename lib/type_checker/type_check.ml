open Poppy_parser.Ast
open Scoping
(* open Sexplib *)
open Core

let access_to_string access : string =
  match access with
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

let rec typ_to_string (typ : typ) : string =
  match typ with
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Function (arg_types, ret_type) ->
    let arg_types_str = List.map arg_types ~f:typ_to_string |> String.concat ~sep:", " in
    Printf.sprintf "function (%s) -> %s" arg_types_str (typ_to_string ret_type)
  | ClassInstance class_name -> Printf.sprintf "class %s" class_name

let initialize_global_scope () = create_new_scope None

let rec check_type (current_scope : scope) (typ : typ) : unit =
  match typ with
  | Int | Bool | Void | String -> () (* Basic types are always valid *)
  | Function (arg_types, ret_type) -> (* Check argument types and return type for functions *)
      List.iter ~f:(check_type current_scope) arg_types;
      check_type current_scope ret_type
  | ClassInstance class_name -> (* Check if the class exists in the current scope *)
      match find_class current_scope class_name with
      | Some _ -> ()
      | None -> raise (Failure ("Undefined class: " ^ class_name))

let rec type_check_expr (current_scope : scope) (expr: expr) : typ = 
  match expr with 
  | IntLiteral _ -> Int
  | BoolLiteral _ -> Bool
  | StringType _ -> String
  | StringLiteral _ -> String
  | VoidType -> Void
  | Unit -> Void
  | Id id -> (*debug*)
    (match find_identifier current_scope id with
     | Some typ -> typ
     | None -> raise (Failure (Printf.sprintf "Unbound variable: %s" id)))
  | BinOp (op, lhs, rhs) ->
    let lhs_typ = type_check_expr current_scope lhs in
    let rhs_typ = type_check_expr current_scope rhs in
    (match op with 
    | Plus | Minus | Times | Div -> (* only Int operations *)
      if equal_typ lhs_typ Int && equal_typ rhs_typ Int then Int
      else raise (Failure "Type error: expected int for arithmetic operation")
    | Lt | Gt | Leq | Geq | Eq | Neq -> (* Int and Bool comparisons *)
      if equal_typ lhs_typ rhs_typ && (equal_typ lhs_typ Int || equal_typ lhs_typ Bool) then Bool
      else raise (Failure "Type error: expected int or bool for comparison operation")
    | And | Or | Xor -> (* only Bool operations *)
      if equal_typ lhs_typ Bool && equal_typ rhs_typ Bool then Bool
      else raise (Failure "Type error: expected bool for logical operation"))
  | Not expr ->
    let expr_typ = type_check_expr current_scope expr in
    if equal_typ expr_typ Bool then Bool
    else raise (Failure "Type error: expected bool for logical operation")
  | ClassInstantiation (class_name, exprs) ->
    (* Check if the class exists in the scope *)
    (match find_class current_scope class_name with
    | Some _ -> ()
    | None -> raise (Failure (Printf.sprintf "Class %s not found" class_name)));
    (* Check the types of the expressions passed as arguments *)
    ignore (List.map exprs ~f:(type_check_expr current_scope));
    ClassInstance class_name
  | ClassMemberAccess (instance_expr, member_name) ->
    (* Check the type of the instance expression *)
    let instance_type = type_check_expr current_scope instance_expr in
    print_endline (Printf.sprintf "instance type %s" (typ_to_string instance_type));
    (* Check if the instance type is a class instance *)
    (match instance_type with
    | ClassInstance class_name ->
      (* Find the class in the scope *)
      (match find_class current_scope class_name with
      | Some class_info ->
        (* Check if the member exists in the class *)
        (match find_method_return_type class_info member_name with
        | Some return_type -> return_type
        | None -> raise (Failure (Printf.sprintf "Member %s not found in class %s" member_name class_name)))
      | None -> raise (Failure (Printf.sprintf "Class %s not found" class_name)))
    | _ -> raise (Failure "The expression is not an instance of a class"))
  | Lambda (params, body) ->
    let param_types = List.map params ~f:(fun (Param (_, Type param_type)) -> param_type) in
    let body_scope = create_new_scope (Some current_scope) in
    List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
      check_type current_scope param_type;
      add_identifier body_scope param_name param_type);
    let body_typ = type_check_expr body_scope body in
    Function (param_types, body_typ)

  | Call (func_name, args) ->
    (match find_identifier current_scope func_name with
    | Some (Function (arg_types, ret_type)) ->
      let args_types = List.map args ~f:(type_check_expr current_scope) in
      if List.equal equal_typ arg_types args_types then
        ret_type
      else
        raise (Failure (Printf.sprintf "Type error: argument types do not match function signature for %s" func_name))
    | _ -> raise (Failure (Printf.sprintf "Type error: function %s not found or not a function" func_name)))
    
  | unimplemented_expression ->
    let sexp = sexp_of_expr unimplemented_expression in
    raise (Failure (Printf.sprintf "Type checking not implemented for this expression: %s" (Sexp.to_string_hum sexp)))

      
let rec type_check_statement (current_scope : scope) (stmt : statement) : unit =
  match stmt with
  | ClassDecl (Id class_name, class_members) ->
    (* Step 1: Create a new class *)
    let class_info = create_new_class_info class_name None in
    add_class current_scope class_name None;
    (* print class info *)
    print_endline (Printf.sprintf "Class %s" class_name);

    (* Step 2 and 3: Iterate over the class members, add them to the class, and check their types *)
    List.iter class_members ~f:(fun member ->
      match member with
      | ClassVar (access, Id var_name, Type var_type) -> (* remove _ before access when it is being used*)
        check_type current_scope var_type; 
        add_member_variable class_info var_name (Variable (Id var_name, var_type));
        (* print class variables *)
        print_endline (Printf.sprintf "%s Variable %s : %s" (access_to_string access) var_name (typ_to_string var_type))
      | ClassMethod (access, Id method_name, params, Type ret_type, body) -> (* remove _ before access when it is being used*)
        check_type current_scope ret_type;

        let method_scope = create_new_scope (Some current_scope) in
        (* print class methods*)
        print_endline (Printf.sprintf "%s  Method %s : %s" (access_to_string access) method_name (typ_to_string ret_type));

        (* Step 4: For each class method, create a new scope, add the method parameters, and check the types of the method body's statements *)
        List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
          check_type current_scope param_type;
          add_identifier method_scope param_name param_type);
        List.iter body ~f:(type_check_statement method_scope);
        add_member_method class_info method_name (Block body) (List.map params ~f:(fun (Param (Id param_name, _)) -> param_name)));
        
  | ClassMemberAssign (instance_expr, member_name, expr) ->
    (* Check the type of the instance expression *)
    let instance_type = type_check_expr current_scope instance_expr in
    (* Check if the instance type is a class instance *)
    (match instance_type with
    | ClassInstance class_name ->
      (* Find the class in the scope *)
      (match find_class current_scope class_name with
      | Some class_info ->
        (* Check if the member exists in the class *)
        (match find_member_variable class_info member_name with
        | Some (Variable (_, member_type)) ->
          (* Check if the expression type matches the member type *)
          let expr_type = type_check_expr current_scope expr in
          if equal_typ expr_type member_type then
            ()
          else
            raise (Failure (Printf.sprintf "Type error: expression type does not match member type in class member assignment"))
        | _ -> raise (Failure (Printf.sprintf "Member %s not found in class %s" member_name class_name)))
      | None -> raise (Failure (Printf.sprintf "Class %s not found" class_name)))
    | _ -> raise (Failure "The expression is not an instance of a class"))
      

  | FuncDecl (Id func_name, params, Type ret_type, body) ->
    check_type current_scope ret_type;
    let arg_types = List.map params ~f:(fun (Param (_, Type param_type)) -> param_type) in
    let func_type = Function (arg_types, ret_type) in
    add_identifier current_scope func_name func_type;
    (* Create a new scope for the function body and add the parameters to this scope *)
    let func_scope = create_new_scope (Some current_scope) in
    List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
      check_type current_scope param_type;
      add_identifier func_scope param_name param_type);
    List.iter body ~f:(type_check_statement func_scope);

  | Let ((Id var_name, Type var_type), expr) ->
    check_type current_scope var_type;
    let expr_typ = type_check_expr current_scope expr in
    if equal_typ expr_typ var_type then
      add_local_variable current_scope var_name (Variable (Id var_name, var_type))
    else
      raise (Failure "Type error: expression type does not match variable type in let statement")
    
  | Assign (id, expr) ->
    let id_typ = type_check_expr current_scope (Id id) in
    let expr_typ = type_check_expr current_scope expr in
    if not (equal_typ id_typ expr_typ) then
      raise (Failure "Type error: assignment type mismatch") 

  | If (cond_expr, true_stmt, false_stmt) ->
    let cond_typ = type_check_expr current_scope cond_expr in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: if condition must be a boolean expression");
    type_check_statement current_scope true_stmt;
    type_check_statement current_scope false_stmt;

  | While (cond_expr, stmt) ->
    let cond_typ = type_check_expr current_scope cond_expr in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: while condition must be a boolean expression");
    type_check_statement current_scope stmt;
  
  | IncrDecr (id, _) ->
    let id_typ = type_check_expr current_scope (Id id) in
    if not (equal_typ id_typ Int) then
      raise (Failure "Type error: increment/decrement operation requires an integer variable")
  
  | Block stmts ->
    let block_scope = create_new_scope (Some current_scope) in
    List.iter stmts ~f:(type_check_statement block_scope);
  
  | Thread stmt ->
    type_check_statement current_scope stmt;
  
  | Return expr ->
    ignore (type_check_expr current_scope expr);

  | Expr expr ->
    ignore (type_check_expr current_scope expr);
    
  | MutexDeclaration (MutexId var_name, Type var_type) ->
    check_type current_scope var_type;
    add_local_variable current_scope var_name (Variable (Id var_name, var_type));

  | unimplement_statement ->
    let sexp = sexp_of_statement unimplement_statement in
    raise (Failure (Printf.sprintf "Type checking not implemented for this statement: %s" (Sexp.to_string_hum sexp)))
  
  
(*
Traverse the Ast *)
let type_check_program (statements : statement list) : unit =
  (* Create the top-level scope for the program *)
  let global_scope = create_new_scope None in

  (* Add built-in types to the global scope *)
  add_identifier global_scope "int" Int;
  add_identifier global_scope "bool" Bool;
  add_identifier global_scope "void" Void;
  add_identifier global_scope "string" String;

  (* Type-check each statement in the program *)
  List.iter statements ~f:(type_check_statement global_scope)

