open Poppy_parser.Ast
open Scoping
open Core

(* Helper Functions *)
let access_to_string access : string =
  match access with
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

let value_to_string value : string = 
  match value with 
  | Variable _ -> "variable"
  | Parameter _ -> "parameter"
  | ReturnValue _ -> "return value"
  | ClassInstance _ -> "class instance"

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

let class_info_to_string class_info =
  let get_typ_from_value var =
    match var with
      | Variable (_, typ)
      | Parameter (_, typ)
      | ReturnValue (_, typ) -> typ
      | ClassInstance (_, _) -> Obj.magic () (* The type of the ClassInstance will be handled differently *)
  in
  let member_variables_str =
    Stdlib.Hashtbl.fold
      (fun key (access, var) acc ->
          let var_typ = get_typ_from_value var in
          acc ^ Printf.sprintf "\n\t%s (%s) : %s" key (access_to_string access) (typ_to_string var_typ))
      class_info.member_variables ""
  in
  let member_methods_str =
    Stdlib.Hashtbl.fold
      (fun key (access, (_body, params)) acc ->
          let params_str = String.concat ~sep:", " (List.map ~f:(fun param -> param) params) in
          acc ^ Printf.sprintf "\n\t%s (%s) : (%s) -> void" key (access_to_string access) params_str)
      class_info.member_methods ""
  in
  Printf.sprintf "%s {%s%s\n}" class_info.class_name member_variables_str member_methods_str
  
  let type_decl_to_string (type_decl: type_decl) : string =
    match type_decl with
    | Type typ -> typ_to_string typ

  let _print_member_variables (class_info: class_info) : unit =
    print_endline (Printf.sprintf "Member variables of class %s:" class_info.class_name);
    Stdlib.Hashtbl.iter
      (fun key (access, value) ->
        let access_str = access_to_string access in
        let value_str = value_to_string value in
        print_endline (Printf.sprintf "  %s (%s) : %s" key access_str value_str))
      class_info.member_variables
      
  let print_member_methods (class_info: class_info) : unit =
    print_endline (Printf.sprintf "Member methods of class %s:" class_info.class_name);
    Stdlib.Hashtbl.iter
      (fun key (access, (stmt, _)) ->
        let access_str = access_to_string access in
        (match stmt with
        | FuncDecl (_, args, ret_type, _) ->
          let args_str = String.concat ~sep:", " (List.map args ~f:(fun (Param (Id name, t)) -> name ^ ": " ^ (type_decl_to_string t))) in
          let ret_type_str = type_decl_to_string ret_type in
          print_endline (Printf.sprintf "  %s (%s) : (%s) -> %s" key access_str args_str ret_type_str)
        | _ -> ()))
      class_info.member_methods
      
  let type_decl_to_typ (type_decl: type_decl) : typ =
    match type_decl with
    | Type typ -> typ

  let extract_arg_types_and_return_type stmt =
    match stmt with
    | FuncDecl(_, args, ret_type, _) ->
      let arg_types = List.map args ~f:(fun (Param(_, t)) -> type_decl_to_typ t) in
      (arg_types, type_decl_to_typ ret_type)
    | _ -> failwith "Not a function declaration"
  
let find_member_access_and_type (current_class_info: class_info) (member_name: string) : (access_modifier * typ) option =
  match Stdlib.Hashtbl.find_opt current_class_info.member_variables member_name with
  | Some (access, Variable (_, typ)) -> Some (access, typ)
  | _ ->
    match Stdlib.Hashtbl.find_opt current_class_info.member_methods member_name with
    | Some (access, (stmt, _)) -> Some (access, find_return_type stmt)
    | None -> None

  let find_method_access_and_type (current_class_info: class_info) (method_name: string) : (access_modifier * typ) option =
    match Stdlib.Hashtbl.find_opt current_class_info.member_methods method_name with
    | Some (access, (stmt, _)) -> 
      (match stmt with
      | FuncDecl(_, _, _, _) ->
        let arg_types, ret_type = extract_arg_types_and_return_type stmt in
        Some (access, Function (arg_types, ret_type))
      | _ -> raise (Failure "cp: Not a function declaration"))
    | _ -> None
  
let is_same_or_subclass_of (target_class_info: class_info option) (base_class_info: class_info option) : bool =
  let rec check_class (class_info : class_info option) : bool =
    match class_info with
    | Some info ->
      if String.equal info.class_name (match base_class_info with Some base_info -> base_info.class_name | None -> "") then
        true
      else
        check_class info.parent_class
    | None -> false
  in
  check_class target_class_info
    
let is_member_accessible (current_class_info: class_info) (target_class_info: class_info) (member_name : string) : bool =
  let check_accessibility access =
    match access with
    | Public -> true
    | Private -> false
    | Protected -> is_same_or_subclass_of (Some target_class_info) (Some current_class_info)
  in
match find_member_access_and_type target_class_info member_name with
  | Some (access, _) -> check_accessibility access
  | None -> false
  
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

let param_to_string (Param (Id param_name, Type param_type)) =
  param_name ^ ": " ^ (typ_to_string param_type)
  
(* Type Check Expressions *)
let rec type_check_expr class_scope (current_scope : scope) (expr: expr) (current_class_info: class_info) (current_method_ret_type: typ option): typ = 
  match expr with 
  | IntLiteral _ -> Int
  | BoolLiteral _ -> Bool
  | StringType _ -> String
  | StringLiteral _ -> String
  | VoidType -> Void
  | Unit -> Void

  | Id id_name ->
    print_endline (Printf.sprintf "Checking identifier %s" id_name);
    print_scope_contents current_scope 0;
    (match find_identifier current_scope id_name current_class_info with
    | Some id_type -> id_type
    | None ->
      (match find_member_variable_type current_class_info id_name with
      | Some var_type -> var_type
      | None -> raise (Failure (Printf.sprintf "Identifier %s not found in current scope or class" id_name))))

  | BinOp (op, e1, e2) ->
    print_endline "Checking binary operator";
    let t1 = type_check_expr class_scope current_scope e1 current_class_info current_method_ret_type in
    let t2 = type_check_expr class_scope current_scope e2 current_class_info current_method_ret_type in
    print_endline (Printf.sprintf "Binary operator types: %s %s" (typ_to_string t1) (typ_to_string t2));
    (match op with
    | Plus | Minus | Times | Div when equal_typ t1 Int && equal_typ t2 Int -> Int
    | Lt | Gt | Leq | Geq when equal_typ t1 Int && equal_typ t2 Int -> Bool
    | Eq | Neq when equal_typ t1 t2 -> Bool
    | And | Or when equal_typ t1 Bool && equal_typ t2 Bool -> Bool
  | _ -> raise (Failure "Type error: binary operator types do not match"))
  
  | ClassInstantiation (var_name, class_name, exprs) ->
    print_endline ("Instantiating class: " ^ class_name);
    (* Check if the class exists in the scope *)
    (match find_class current_scope class_name with
    | Some class_info ->
        (* Check the types of the expressions passed as arguments *)
        ignore (List.map exprs ~f:(type_check_expr class_scope current_scope));
        (* Create an instance of the class *)
        let instance_value = create_instance class_info in
        (* Add the class instance to the scope *)
        add_local_variable current_scope var_name instance_value;
        ClassInstance class_name
    | None -> raise (Failure (Printf.sprintf "CI: Class %s not found" class_name)))
  
  | ClassMemberAccess (instance_expr, member_name) ->
    print_endline ("Accessing member: " ^ member_name);
    (* Check the type of the instance expression *)
    let instance_type = type_check_expr class_scope current_scope instance_expr current_class_info current_method_ret_type in
    (* Check if the instance type is a class instance *)
    (match instance_type with
    | ClassInstance class_name ->
      (* Find the class in the scope *)
      (match find_class current_scope class_name with
      | Some class_info ->
        (* Check if the member exists in the class *)
        (match find_member_access_and_type class_info member_name with
        | Some (access, typ) ->
          (* Check if the access modifier allows access to the member from the current scope *)
          if is_member_accessible current_class_info class_info member_name then
            typ
          else
            raise (Failure (Printf.sprintf "Access error: %s member %s is not accessible from the current scope" (access_to_string access) member_name))
        | None -> raise (Failure (Printf.sprintf "ClassMemberAccess: Member %s not found in class %s" member_name class_name)))
      | None -> raise (Failure (Printf.sprintf "CMAC: Class %s not found" class_name)))
    | _ -> raise (Failure "The expression is not an instance of a class"))

  | Lambda (params, body) ->
    let param_types = List.map params ~f:(fun (Param (_, Type param_type)) -> param_type) in
    let body_scope = create_new_scope (Some current_scope) in
    List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
      check_type current_scope param_type;
      add_identifier body_scope param_name param_type);
    let body_typ = type_check_expr class_scope body_scope body current_class_info current_method_ret_type in
    Function (param_types, body_typ)

  | Call (func_name, args) ->
    print_endline ("Calling function: " ^ func_name);
    (match find_identifier current_scope func_name current_class_info with
    | Some (Function (arg_types, ret_type)) ->
      let args_types = List.map args ~f:(fun arg -> type_check_expr class_scope current_scope arg current_class_info current_method_ret_type) in
      if List.equal equal_typ arg_types args_types then
        ret_type
      else
        raise (Failure (Printf.sprintf "Type error: argument types do not match function signature for %s" func_name))
    | _ -> raise (Failure (Printf.sprintf "Type error: function %s not found or not a function" func_name)))
    
  | This ->
    print_endline ("Using 'this' keyword");
    ClassInstance current_class_info.class_name

  | InstanceMethodCall (expr, method_name, args) ->
    print_endline ("Calling instance method: " ^ method_name);
    (* Check the type of the instance expression *)
    let instance_type = type_check_expr class_scope current_scope expr current_class_info current_method_ret_type in
    (* Check if the instance type is a class instance *)
    (match instance_type with
    | ClassInstance class_name ->
      (* Find the class in the scope *)
      (match find_class current_scope class_name with
      | Some class_info ->
        print_member_methods class_info;
        (* Check if the member exists in the class *)
        (match find_method_access_and_type class_info method_name with
        | Some (access, Function (arg_types, ret_type)) ->  (* Ensure it's a Function type *)
          print_endline (access_to_string access);
          print_endline (method_name);
          print_endline ((typ_to_string (Function (arg_types, ret_type))));
          (* Check if the access modifier allows access to the member from the current scope *)
          if is_member_accessible current_class_info class_info method_name then
            (* Check if the number of arguments matches the number of parameters *)
              if phys_equal (List.length arg_types) (List.length args) then
                (* Check if the types of the arguments match the types of the parameters *)
                  let args_types = List.map args ~f:(fun arg -> type_check_expr class_scope current_scope arg current_class_info current_method_ret_type) in
                  print_endline (Printf.sprintf "Member %s has access modifier %s" method_name (access_to_string access));
              if List.equal equal_typ arg_types args_types then
                ret_type
              else
                raise (Failure (Printf.sprintf "Type error: argument types do not match function signature for %s" method_name))
            else
              raise (Failure (Printf.sprintf "Type error: number of arguments does not match function signature for %s" method_name))
          else
            raise (Failure (Printf.sprintf "Access error: %s member %s is not accessible from the current scope" (access_to_string access) method_name))
        | Some (_, _) -> raise (Failure (Printf.sprintf "Type error: member %s is not a function" method_name))
        | None -> raise (Failure (Printf.sprintf "InstanceMethodCall: Member %s not found in class %s" method_name class_name)))
      | None -> raise (Failure (Printf.sprintf "IMC: Class %s not found" class_name)))
    | _ -> raise (Failure "The expression is not an instance of a class"))
  
  | unimplemented_expression ->
    let sexp = sexp_of_expr unimplemented_expression in
    raise (Failure (Printf.sprintf "Type checking not implemented for this expression: %s" (Sexp.to_string_hum sexp)))

  (* TODO unimplemented: expr, not, InstanceMethodCall *)

(* Type Check Statements *)
let rec type_check_statement class_scope (current_scope : scope) (stmt : statement) (current_class_info : class_info) (current_method_ret_type: typ option): unit =
  let type_check_class_members class_scope current_scope class_name class_members =
    print_endline (Printf.sprintf "Type checking class %s" class_name);
    let class_info = find_class current_scope class_name in
    (match class_info with
    | Some class_info ->
      print_endline(Printf.sprintf "found class %s" class_name);
      List.iter class_members ~f:(fun member ->
        match member with
        | ClassMethod (_, Id method_name, params, _, body) ->
          (match find_member_method class_info method_name with
          | Some (_, (FuncDecl (_, _, ret_type, _), _)) ->
            print_endline(Printf.sprintf "found method %s" method_name);
            let method_scope = create_new_scope (Some class_scope) in
            add_identifier method_scope "this" (ClassInstance class_name);
            List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
              print_endline (Printf.sprintf "Adding param %s of type %s" param_name (typ_to_string param_type));
              add_identifier method_scope param_name param_type);   
            List.iter body ~f:(fun stmt -> print_endline(string_of_statement stmt));
            List.iter body ~f:(fun stmt -> type_check_statement class_scope method_scope stmt class_info (Some (type_decl_to_typ ret_type)));
            | None -> ()
          | _ -> raise (Failure (Printf.sprintf "Unhandled case in method member matching")))
        | _ -> ()
      )
    | None -> raise (Failure (Printf.sprintf "CD: Class %s not found" class_name)))
  in
  match stmt with
  | ClassDecl (Id class_name, class_members) ->
    print_endline "entering 1st pass class decl";
    if phys_equal current_class_info.class_name "" then
      (* Step 1: Create a new class *)
      let current_class_info = create_new_class_info class_name None in      
      (* Create a new scope for the class *)
      let class_scope = create_new_scope (Some current_scope) in
      
      (* Step 2 and 3: Iterate over the class members, add them to the class, and check their types *)
      List.iter class_members ~f:(fun member ->
        match member with
        | ClassVar (access, Id var_name, Type var_type) ->
          print_endline ("Adding member variable: " ^ var_name);
          check_type class_scope var_type ; 
          add_member_variable current_class_info var_name (access, Variable (Id var_name, var_type));
          
        | ClassMethod (access, Id method_name, params, Type ret_type, body) -> 
          print_endline ("Adding member method: " ^ method_name);
          check_type class_scope ret_type;
          let method_scope = create_new_scope (Some class_scope) in
          add_identifier method_scope "this" (ClassInstance class_name); (* Add the "this" identifier to the method scope *)

          List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
            check_type class_scope param_type;
            add_identifier method_scope param_name param_type);
            (* Step 4: Add the method to the class *)
            add_member_method current_class_info method_name (access, (FuncDecl (Id method_name, params, Type ret_type, body), List.map params ~f:(fun (Param (Id param_name, _)) -> param_name)));
            );            
      (* Step 5: Add the class to the scope *) 
      print_endline "adding class to scope dfa";
      let global_scope = get_global_scope current_scope in
      add_class global_scope class_name current_class_info;
    else
      (* Type-check method bodies *)
      print_endline "entering 2nd pass class decl";
      type_check_class_members class_scope current_scope class_name class_members;
  
  | ClassMemberAssign (instance_expr, member_name, rhs_expr) ->
    print_endline "entering class member assign";
    let instance_type = type_check_expr class_scope current_scope instance_expr current_class_info current_method_ret_type in
    (match instance_type with
    | ClassInstance class_name ->
      print_endline ("finding class: "^ class_name);
      (match find_class current_scope class_name with
      | Some class_info ->
        (match find_member_access_and_type class_info member_name with
        | Some (access, member_type) ->
          print_endline ("lhs: " ^ (access_to_string access) ^ " " ^ member_name ^ " " ^ (typ_to_string member_type));
          if is_member_accessible current_class_info class_info member_name then
            let rhs_type = type_check_expr class_scope current_scope rhs_expr current_class_info current_method_ret_type in
            if equal_typ member_type rhs_type then
              ()
            else
              raise (Failure (Printf.sprintf "Type error: assignment to member %s has incompatible types" member_name))
          else
            raise (Failure (Printf.sprintf "Access error: %s member %s is not accessible from the current scope" (access_to_string access) member_name))
        | None -> raise (Failure (Printf.sprintf "ClassMemberAssign: Member %s not found in class %s" member_name class_name)))
      | None -> raise (Failure (Printf.sprintf "CMA: Class %s not found" class_name)))
    | _ -> raise (Failure "The expression is not an instance of a class"))
    
  | FuncDecl (Id func_name, params, Type ret_type, body) ->
    print_endline "entering func decl";
    check_type current_scope ret_type;
    let arg_types = List.map params ~f:(fun (Param (_, Type param_type)) -> param_type) in
    let func_type = Function (arg_types, ret_type) in
    add_identifier current_scope func_name func_type;
    (* Create a new scope for the function body and add the parameters to this scope *)
    let func_scope = create_new_scope (Some current_scope) in
    List.iter params ~f:(fun (Param (Id param_name, Type param_type)) ->
      check_type current_scope param_type;
      add_identifier func_scope param_name param_type);
       (* Print the parameters *)
      print_endline "Function parameters:";
      List.iter params ~f:(fun param -> print_endline (param_to_string param));
      List.iter body ~f:(fun stmt -> type_check_statement class_scope func_scope stmt current_class_info (Some ret_type));

  | Let ((Id var_name, Type var_type), expr) ->
    print_endline "entering let";
    check_type current_scope var_type;
    let expr_typ = type_check_expr class_scope current_scope expr current_class_info current_method_ret_type in
    if equal_typ expr_typ var_type then
      add_local_variable current_scope var_name (Variable (Id var_name, var_type))
    else
      raise (Failure (Printf.sprintf"Type error: expression type does not match variable type in let statement. Variable: %s has type %s but is expecting %s" var_name (typ_to_string var_type) (typ_to_string expr_typ)));
      
  | Assign (id, expr) ->
    print_endline ("entering assign of id: " ^ id);
    (match find_identifier current_scope id current_class_info with
    | Some var_type ->
      let expr_type = type_check_expr class_scope current_scope expr current_class_info current_method_ret_type in
      if not (equal_typ var_type expr_type) then
        raise (Failure (Printf.sprintf "Type error: Mismatch in assignment. Expected %s, got %s" (string_of_typ var_type) (string_of_typ expr_type)))
    | None -> raise (Failure (Printf.sprintf "Undeclared identifier %s" id)))

  | If (cond_expr, true_stmt, false_stmt) ->
    print_endline "entering if";
    let cond_typ = type_check_expr class_scope current_scope cond_expr current_class_info current_method_ret_type in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: if condition must be a boolean expression");
    type_check_statement class_scope current_scope true_stmt current_class_info current_method_ret_type;
    type_check_statement class_scope current_scope false_stmt current_class_info current_method_ret_type;

  | While (cond_expr, stmt) ->
    let cond_typ = type_check_expr class_scope current_scope cond_expr current_class_info current_method_ret_type in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: while condition must be a boolean expression");
    type_check_statement class_scope current_scope stmt current_class_info current_method_ret_type;
  
  | IncrDecr (id, _) ->
    let id_typ = type_check_expr class_scope current_scope (Id id) current_class_info current_method_ret_type in
    if not (equal_typ id_typ Int) then
      raise (Failure "Type error: increment/decrement operation requires an integer variable")
  
  | Block stmts ->
    print_endline "entering block";
    let block_scope = create_new_scope (Some current_scope) in
    List.iter stmts ~f:(fun stmts -> type_check_statement class_scope block_scope stmts current_class_info current_method_ret_type);
  
  | Thread stmt ->
    type_check_statement class_scope current_scope stmt current_class_info current_method_ret_type;
  
  | Return expr ->
    print_endline "entering return";
    let expr_type = type_check_expr class_scope current_scope expr current_class_info current_method_ret_type in
    print_endline (Printf.sprintf "type of expr_type: %s" (typ_to_string expr_type));
    print_endline (Printf.sprintf "type of current_method_ret_type: %s" (match current_method_ret_type with Some typ -> typ_to_string typ | None -> "None"));
    (match current_method_ret_type with
    | Some ret_type ->
      print_endline (Printf.sprintf "type of ret_type: %s" (typ_to_string ret_type));
      if not (equal_typ ret_type expr_type) then
        raise (Failure (Printf.sprintf "Type error: Mismatch in return type. Expected %s, got %s" (string_of_typ ret_type) (string_of_typ expr_type)))
    | None -> raise (Failure "Return statement outside of a function"))
    
  | Expr expr ->
    print_endline "entering expr";
    print_endline (Sexp.to_string_hum (sexp_of_expr expr));
    ignore (type_check_expr class_scope current_scope expr current_class_info current_method_ret_type);
    
  | MutexDeclaration (MutexId var_name, Type var_type) ->
    check_type current_scope var_type;
    add_local_variable current_scope var_name (Variable (Id var_name, var_type));

  | unimplement_statement ->
    let sexp = sexp_of_statement unimplement_statement in
    raise (Failure (Printf.sprintf "Type checking not implemented for this statement: %s" (Sexp.to_string_hum sexp)))
  (* unimplemented: for, mutexlock, mutexunlock *)
  
(*Traverse the Ast *)
let type_check_program (statements : statement list) : unit =
  (* Create the top-level scope for the program *)
  let global_scope = create_new_scope None in

  (* Type-check each statement in the program *)
  let empty_class_info = { class_name = ""; 
                            parent_class = None; 
                            class_scope = global_scope;
                            member_variables = Stdlib.Hashtbl.create 0;
                            member_methods = Stdlib.Hashtbl.create 0
                          } in

  (* First pass: Declare classes and add them to the global scope *)
  print_endline "~~~~~~~~ First pass ~~~~~~~~";
  List.iter statements ~f:(fun stmt ->
    print_endline ("First pass: " ^ string_of_statement stmt);
    match stmt with
    | ClassDecl _ -> type_check_statement global_scope global_scope stmt empty_class_info None
    | _ -> ()
  );

  (* Second pass: Type-check the rest of the statements, including method bodies *)
  print_endline "~~~~~~~~ Second pass ~~~~~~~~";
  List.iter statements ~f:(fun stmt ->
    print_endline ("Second pass: " ^ string_of_statement stmt);
    match stmt with
    | ClassDecl _ -> ()
    | _ -> type_check_statement global_scope global_scope stmt empty_class_info None
  );