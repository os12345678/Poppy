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

let rec llvm_type_of_ast_type context = function
| Ast.Int-> i64_type context
| Ast.Bool -> i1_type context
| Ast.Void -> void_type context (* void type not working *)
| Ast.String -> pointer_type (i8_type context) 
| Ast.Function (param_types, return_type) ->
  let llvm_param_types = Array.of_list (List.map (fun typ -> llvm_type_of_ast_type context typ) param_types) in
  let llvm_return_type = llvm_type_of_ast_type context return_type in
  function_type llvm_return_type llvm_param_types
;;

let add_implicit_return return_type last_value_opt =
  match last_value_opt with
  | None -> () (* Do not add an implicit return if the last statement was a return *)
  | Some last_value ->
    match return_type with
    | Ast.Type Ast.Void ->
      ignore (build_ret_void builder)
    | Ast.Type Ast.Int ->
      ignore (build_ret (last_value) builder)
    | Ast.Type Ast.String ->
      ignore (build_ret (last_value) builder)
    | Ast.Type Ast.Bool ->
      ignore (build_ret (last_value) builder)
    | Ast.Type (Ast.Function _) ->
      (* No implicit return for lambda functions *)
      ()
    ;;

let is_return_statement (stmt: Ast.statement) : bool =
  match stmt with
  | Ast.Return _ -> true
  | _ -> false
;;

let is_pointer (llvm_type: lltype) : bool =
  classify_type llvm_type = TypeKind.Pointer
;;

let declare_print_function the_module =
  let void_type = Llvm.void_type (Llvm.module_context the_module) in
  let string_type = Llvm.pointer_type (Llvm.i8_type (Llvm.module_context the_module)) in
  let int_ptr_type = Llvm.pointer_type (Llvm.i64_type (Llvm.module_context the_module)) in
  let print_type = Llvm.function_type void_type [| string_type; int_ptr_type |] in
  Llvm.declare_function "print" print_type the_module
;;
let print_function = declare_print_function the_module ;;
    
(* Codegen Expressions *)
let rec codegen_expr = function
  | Ast.IntLiteral i -> const_int (i64_type context) i

  | Ast.BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)

  | Ast.StringLiteral s ->
    let str = Llvm.build_global_stringptr s "global_string" builder in
    str

  | Ast.Id s ->
    (match lookup_var_in_scopes s with
     | Some v -> v
     | None -> raise (Failure ("Undefined variable: " ^ s)))

  | Ast.BinOp (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin 
      match op with
      | Ast.Plus ->
        let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
        let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
        build_add lhs_val rhs_val "addtmp" builder
      | Ast.Minus -> 
        let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
        let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
        build_sub lhs_val rhs_val "subtmp" builder
      | Ast.Times -> 
        let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
        let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
        build_mul lhs_val rhs_val "multmp" builder
      | Ast.Div -> 
        let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
        let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
        build_sdiv lhs_val rhs_val "divtmp" builder
      | Ast.Lt -> 
        let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
        let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
        build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder
        | Ast.Gt ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder
        | Ast.Leq ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder
        | Ast.Geq ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder
        | Ast.Eq ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder
        | Ast.Neq ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder
        | Ast.And ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_and lhs_val rhs_val "andtmp" builder
        | Ast.Or ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_or lhs_val rhs_val "ortmp" builder
        | Ast.Xor ->
          let lhs_val = if is_pointer (type_of lhs_val) then build_load lhs_val "loadtmp" builder else lhs_val in
          let rhs_val = if is_pointer (type_of rhs_val) then build_load rhs_val "loadtmp" builder else rhs_val in
          build_xor lhs_val rhs_val "xortmp" builder
    end

  | Ast.Not e ->
    let e_val = codegen_expr e in
    let i = build_not e_val "nottmp" builder in
    build_zext i (i1_type context) "booltmp" builder

    | Ast.Call (fnname, args) ->
      let callee = 
        match lookup_function fnname the_module with 
        | Some callee -> callee
        | None -> 
          match Llvm.lookup_function fnname the_module with
          | Some ext_callee -> ext_callee
          | None ->
            Llvm.iter_functions (fun f -> Printf.printf "Function: %s\n" (Llvm.value_name f)) the_module;
            raise (Failure ("unknown function referenced: " ^ fnname))
      in
      let args_array = Array.of_list args in
      let params = params callee in
      if Array.length params <> Array.length args_array then
        raise (Failure "incorrect # arguments passed");
      let args = Array.mapi (fun i arg ->
        let arg_val = codegen_expr arg in
        match classify_type (type_of arg_val) with
        | TypeKind.Pointer when fnname = "print" && i = 0 ->
          arg_val
        | TypeKind.Pointer when not (fnname = "print") ->
          build_load arg_val "loadtmp" builder
        | TypeKind.Integer when fnname = "print" && i = 1 ->
          build_sext arg_val (Llvm.pointer_type (Llvm.i64_type (Llvm.global_context ()))) "sexttmp" builder
        | _ ->
          arg_val
      ) args_array in
      let call_inst = build_call callee args "" builder in
      call_inst
    
  
  | unimplement_expression ->
    let sexp = Ast.sexp_of_expr unimplement_expression in
    let expr_str = Sexp.to_string_hum sexp in
    raise (Failure ("expression not implemented: " ^ expr_str))

(* Codegen Statement *)
let rec codegen_block (block: Ast.statement list) : llvalue option =
  match block with
  | [] -> Some (const_int (i64_type context) 0)
  | [s] -> begin
    let last_value = codegen_statement s in
    if is_return_statement s then
      None (* return statement within a block (i.e return add(i+1); not working)*)
    else
      Some last_value
  end
  | s::rest -> begin
    ignore (codegen_statement s);
    codegen_block rest
  end

and codegen_statement : Ast.statement -> llvalue = function
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
        let llvm_return_type = match return_type with Ast.Type typ -> llvm_type_of_ast_type context typ in
        let llvm_arg_types = List.map (fun (Ast.Param (_, arg_type)) -> match arg_type with Ast.Type typ -> llvm_type_of_ast_type context typ) args in
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
          let last_value = codegen_block body in
          if is_main_function then
            ignore (build_ret (const_int (i64_type context) 0) builder)
          else
            add_implicit_return return_type last_value;
        end;
        position_at_end bb builder;
        let popped_scope = Stack.pop scopes in
        Hashtbl.clear popped_scope;
        the_function
    end

  | Ast.Assign (id, expr) ->
    let value = codegen_expr expr in
    let alloca = match lookup_var_in_scopes id with
  | Some v -> v
  | None -> raise (Failure ("Undefined variable: " ^ id))
    in
    ignore (build_store value alloca builder);
    const_int (i64_type context) 0
    
  | Ast.Expr expr -> 
    ignore (codegen_expr expr);
    const_int (i64_type context) 0

  | Ast.Return expr ->
    let ret_value = codegen_expr expr in
    ignore (build_ret ret_value builder);
    const_int (i64_type context) 0

  | Ast.Let ((id_decl, _), expr) ->
    let id = match id_decl with Ast.Id id_str -> id_str in
    let value = codegen_expr expr in
    let alloca = build_alloca (Llvm.type_of value) id builder in
    ignore (build_store value alloca builder);
    ignore (add_var_to_current_scope id alloca);
    const_int (i64_type context) 0

  | Ast.If (cond, then_, else_) ->
    let bool_val = codegen_expr cond in
    let zero_val = const_int (type_of bool_val) 0 in
    let cond_val = build_icmp Icmp.Ne bool_val zero_val "ifcond" builder in

    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then" the_function in
    position_at_end then_bb builder;
    ignore(codegen_statement then_);
    
    let then_val = const_int (i1_type context) 1 in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block context "else" the_function in
    position_at_end else_bb builder;
    ignore(codegen_statement else_);

    let else_val = const_int (i1_type context) 0 in
    let new_else_bb = insertion_block builder in
    
    let merge_bb = append_block context "ifcont" the_function in
    position_at_end merge_bb builder;
    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
    let phi = build_phi incoming "iftmp" builder in
    position_at_end start_bb builder;
    ignore (build_cond_br cond_val then_bb else_bb builder);
    position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
    position_at_end new_else_bb builder; ignore (build_br merge_bb builder);
    position_at_end merge_bb builder;
    phi
  
| Ast.Block stmt_list ->
  enter_scope ();
  let block_result = codegen_block stmt_list in
  exit_scope ();
  (match block_result with
    | None -> raise (Failure "block result is None")
    | Some llvalue -> llvalue)

  | unimplemented_statement -> 
    let sexp = Ast.sexp_of_statement unimplemented_statement in
    let stmt_str = Sexp.to_string_hum sexp in
    raise (Failure ("statement not implemented: " ^ stmt_str))


let codegen_ast (ast : Ast.statement list) : llmodule =
  List.iter (fun stmt -> ignore (codegen_statement stmt)) ast;
  the_module

let codegen_ast_to_string (ast : Ast.statement list) : string =
  let module_ = codegen_ast ast in
  string_of_llmodule module_

(* Function to link core library to the main module *)
let link_core_library the_module =
  let bindings = "/Users/oliver/Documents/University/Honours/poppy/core_lib/bindings.ll" in

  (* Parse the core library LLVM IR *)
  let context = global_context () in
  let corelib_buf = MemoryBuffer.of_file bindings in
  let corelib_module =
    try
      Llvm_irreader.parse_ir context corelib_buf
    with
    | Llvm_irreader.Error msg ->
      Printf.printf "Error parsing core library: %s\n" msg;
      raise (Failure "Error parsing core library")
    in

  Llvm.iter_functions (fun f -> Printf.printf "Core lib function: %s\n" (Llvm.value_name f)) corelib_module;

  (* Link the core library to the main module *)
  Llvm_linker.link_modules' the_module corelib_module;
  print_endline "Core library linked to the main module."

 