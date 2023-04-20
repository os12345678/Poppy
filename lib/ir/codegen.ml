open Poppy_parser
open Llvm
open Sexplib

(* Module and Context Setup *)
let context = global_context ()
let builder = builder context
let the_module = create_module context "poppy_compiler"
let scopes = Stack.create ()

let function_protos: (string, (string * Ast.func_param list * Ast.type_decl) option) Hashtbl.t = Hashtbl.create 50

(* Helper Functions *)
let counter = ref 0
;;

let generate_unique_id () =
  let time = Unix.gettimeofday () in
  let id = Printf.sprintf "%.0f_%d" time !counter in
  counter := !counter + 1;
  id
;;

let string_of_llmodule m =
  let s = string_of_llmodule m in
  dispose_module m;
  s
;;

let enter_scope () =
  Stack.push (Hashtbl.create 10) scopes
;;

let exit_scope () =
  ignore (Stack.pop scopes)
;;

let current_scope () =
  Stack.top scopes
;;

let add_var_to_current_scope id alloca =
  let scope = current_scope () in
  Hashtbl.add scope id alloca;
  alloca  
;;

  let lookup_var_in_scopes id =
    let result = ref None in
    Stack.iter (fun scope ->
      if Hashtbl.mem scope id then result := Some (Hashtbl.find scope id)
    ) scopes;
    !result
;;

let find_function_prototype function_name =
  try
    let the_function = Hashtbl.find function_protos function_name in
    Some the_function
  with Not_found ->
    None
;;

let is_valid_main_function_signature args return_type =
  (* Check if the return type is int *)
  let valid_return_type = match return_type with
    | Ast.Type Ast.Int -> true
    | _ -> false
  in
  (* Check if the arguments are empty *)
  let valid_args = args = [] in
  valid_return_type && valid_args
;;

let is_void llvm_type = classify_type llvm_type = TypeKind.Void;;

let llvm_type_of_ast_type = function
| Ast.Int-> i64_type context
| Ast.Bool -> i1_type context
| Ast.Void -> void_type context (* void type not working *)
| Ast.String -> pointer_type (i8_type context) 

(* Codegen Expressions *)
let rec codegen_expr = function
  | Ast.IntLiteral i -> const_int (i64_type context) i

  | Ast.BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)

  | Ast.StringLiteral s -> const_stringz context s

  | Ast.Id s ->
    (match lookup_var_in_scopes s with
     | Some v -> v
     | None -> raise (Failure ("Undefined variable: " ^ s)))

  | Ast.BinOp (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin 
      match op with
      | Ast.Plus -> build_add lhs_val rhs_val "addtmp" builder
      | Ast.Minus -> build_sub lhs_val rhs_val "subtmp" builder
      | Ast.Times -> build_mul lhs_val rhs_val "multmp" builder
      | Ast.Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      | Ast.Lt -> 
        let i = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Gt ->
        let i = build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Leq ->
        let i = build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Geq ->
        let i = build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Eq ->
        let i = build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Neq ->
        let i = build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.And ->
        let i = build_and lhs_val rhs_val "andtmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Or ->
        let i = build_or lhs_val rhs_val "ortmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Ast.Xor ->
        let i = build_xor lhs_val rhs_val "xortmp" builder in
        build_zext i (i1_type context) "booltmp" builder
    end

  | Ast.Not e ->
    let e_val = codegen_expr e in
    let i = build_not e_val "nottmp" builder in
    build_zext i (i1_type context) "booltmp" builder

  | Ast.Call (callee, args) ->
    let callee = 
      match lookup_function callee the_module with 
      | Some callee -> callee
      | None -> raise (Failure "unknown function referenced")
    in
    let params = params callee in
    let args_array = Array.of_list args in
    if Array.length params == Array.length args_array then () else 
      raise (Failure "incorrect # arguments passed");
    let args = Array.map (fun arg ->
      let arg_val = codegen_expr arg in
      match classify_type (type_of arg_val) with
      | TypeKind.Pointer ->
        build_load arg_val "loadtmp" builder
      | _ ->
        arg_val
    ) args_array in
    build_call callee args "calltmp" builder
  
  | unimplement_expression ->
    let sexp = Ast.sexp_of_expr unimplement_expression in
    let expr_str = Sexp.to_string_hum sexp in
    raise (Failure ("expression not implemented: " ^ expr_str))

(* Codegen Statement *)
let rec codegen_block (block: Ast.statement list) : Ast.statement option =
  match block with
  | [] -> None
  | [s] -> begin
    ignore (codegen_statement s);
    Some s
  end 
  | s::rest -> begin
    ignore (codegen_statement s);
    codegen_block rest
  end


and codegen_statement : Ast.statement -> llvalue option = function
  | Ast.FuncDecl (Ast.Id name, args, return_type, body) ->
    (* Check if the function is the main function *)
    let is_main_function = (name = "main") in
    (* If it is the main function, ensure it has the correct signature *)
    if is_main_function && (not (is_valid_main_function_signature args return_type)) then
      raise (Failure "Invalid main function signature");
    begin
      match find_function_prototype name with
      | Some _ ->
        raise (Failure ("Function " ^ name ^ " already exists."))
      | None ->
        let llvm_return_type = match return_type with Ast.Type typ -> llvm_type_of_ast_type typ in
        let llvm_arg_types = List.map (fun (Ast.Param (_, arg_type)) -> match arg_type with Ast.Type typ -> llvm_type_of_ast_type typ) args in
        let arg_types = Array.of_list llvm_arg_types in
        let ft = function_type llvm_return_type arg_types in
        let the_function = declare_function name ft the_module in
        Hashtbl.add function_protos name (Some (name, args, return_type));
        let n = Array.length (params the_function) in
        if n == List.length args then () else
          raise (Failure "incorrect # arguments passed");
        let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
        Stack.push (named_values) scopes;
        for i = 0 to n - 1 do
          let Ast.Param (Ast.Id arg, _) = List.nth args i in
          let param = param the_function i in
          set_value_name arg param;
          Hashtbl.add named_values arg param
        done;
        let bb = append_block context "entry" the_function in
        position_at_end bb builder;
        begin
          let last_statement = codegen_block body in
          if not (match last_statement with Some (Ast.Return _) -> true | _ -> false) then
            match return_type with
            | Ast.Type Ast.Void ->
              ignore (build_ret_void builder)
            | Ast.Type Ast.Int ->
              ignore (build_ret (const_int (i64_type context) 0) builder)
            | Ast.Type Ast.String ->
              ignore (build_ret (const_pointer_null (pointer_type (i8_type context))) builder)
            | _ ->
              ()
        end;
        position_at_end bb builder;
        let popped_scope = Stack.pop scopes in
        Hashtbl.clear popped_scope;
        Some the_function
    end

  | Ast.Assign (id, expr) ->
    let value = codegen_expr expr in
    let alloca = match lookup_var_in_scopes id with
  | Some v -> v
  | None -> raise (Failure ("Undefined variable: " ^ id))
    in
    ignore (build_store value alloca builder);
    None
    
  | Ast.Expr expr -> 
    ignore (codegen_expr expr);
    None

  | Ast.Return expr ->
    let ret_value = codegen_expr expr in
    ignore (build_ret ret_value builder);
    None

    
  | Ast.Let ((id_decl, _), expr) ->
    let id = match id_decl with Ast.Id id_str -> id_str in
    let value = codegen_expr expr in
    let alloca = build_alloca (Llvm.type_of value) id builder in
    ignore (build_store value alloca builder);
    ignore (add_var_to_current_scope id alloca);
    None

  (* | Ast.Block stmts ->
    codegen_block stmts;
    None *)

  | unimplemented_statement -> 
    let sexp = Ast.sexp_of_statement unimplemented_statement in
    let stmt_str = Sexp.to_string_hum sexp in
    raise (Failure ("statement not implemented: " ^ stmt_str))

let codegen_ast (ast : Ast.statement list) : llmodule =
  enter_scope ();
  List.iter (fun stmt -> ignore (codegen_statement stmt)) ast;
  exit_scope ();
  the_module

let codegen_ast_to_string (ast : Ast.statement list) : string =
  let module_ = codegen_ast ast in
  string_of_llmodule module_