(* open Llvm
open Poppy_parser
open Scoping
(* open Sexplib *)
open Core
(* open Poppy_type_checker *)
open Codegen_util

exception Codegen_error of string

(* ##################### Module and Context Creation ######################## *)
let context = context
let builder = builder
let the_module = the_module
let named_values : (string, llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10
let function_protos: (string, (string * Ast.func_param list * Ast.type_decl) option) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 50
let class_protos: (string, class_info) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 50
let expr_types : (llvalue, value) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10


(* ############################# Codegen Expr ############################### *)
let rec codegen_expr (expr: Ast.expr) : Llvm.llvalue =
  match expr with
| IntLiteral i -> const_int (i64_type context) i
| BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)
| StringLiteral s ->
  let str = Llvm.build_global_stringptr s "global_string" builder in
  str
| VoidType -> const_null (void_type context)
| Unit -> const_null (void_type context)

| Id id ->
  (match find_named_value id named_values with
   | Some value -> value
   | None -> failwith (Printf.sprintf "Unbound variable: %s" id))

| BinOp (op, e1, e2) ->
  let left = codegen_expr e1 in
  let right = codegen_expr e2 in
  codegen_binop op left right

| Call (func_name, args) ->
  let llvm_args = List.map ~f:(fun arg -> codegen_expr arg) args in
  codegen_call func_name llvm_args func_map
  
(* | ClassInstantiation (var_name, class_name, exprs) ->
  let llvm_args = List.map ~f:(fun arg -> codegen_expr arg) exprs in
  codegen_class_instantiation scope var_name class_name llvm_args builder

| ClassMemberAccess (instance_expr, member_name) ->
  let instance_value = codegen_expr instance_expr in
  codegen_class_member_access instance_value member_name scope class_info *)

  | unimplement_expression ->
    let sexp = Ast.sexp_of_expr unimplement_expression in
    let expr_str = Sexp.to_string_hum sexp in
    raise (Failure ("expression not implemented: " ^ expr_str))

(* ########################## Codegen Statement ############################# *)

let rec codegen_block (block: Ast.statement list) : llvalue option =
  match block with
  | [] -> Some (const_int (i64_type context) 0)
  | [s] -> begin
    let last_value = codegen_statement s in
    if is_return_statement s then
      None 
    else
      Some last_value
  end
  | s::rest -> begin
    ignore (codegen_statement s);
    codegen_block rest
  end
and codegen_statement (stmt: Ast.statement) : llvalue =
  match stmt with
  | Ast.FuncDecl (Ast.Id name, args, return_type, body) ->
    (* Convert AST types to LLVM types *)
    let llvm_return_type = match return_type with Ast.Type typ -> llvm_type_of_typ context typ in
    let llvm_arg_types = List.map ~f:(fun (Ast.Param (_, arg_type)) -> match arg_type with Ast.Type typ -> llvm_type_of_typ context typ) args in
    (* Define the function type and the function itself *)
    let ft = function_type llvm_return_type (Array.of_list llvm_arg_types) in
    let the_function = declare_function name ft the_module in
    (* Setup function parameters *)
    let named_values:(string, llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10 in
    Array.iteri (params the_function) ~f:(fun i param ->
      let Ast.Param (Ast.Id arg, _) = List.nth_exn args i in
      set_value_name arg param;
      Stdlib.Hashtbl.add named_values arg param
    );
    (* Create a new basic block and position builder at the end *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    (* Generate code for the function body *)
    begin match codegen_block body with
      | None -> ignore (build_ret (const_int (i64_type context) 0) builder)
      | Some return_val -> ignore (build_ret return_val builder)
    end;
    (* Position builder at the end of the basic block and return the function *)
    position_at_end bb builder;
    the_function
    
  | unimplemented_statement -> 
    let sexp = Ast.sexp_of_statement unimplemented_statement in
    let stmt_str = Sexp.to_string_hum sexp in
    raise (Failure ("statement not implemented: " ^ stmt_str))

(* ############################ Codegen Program ############################# *)
let codegen_ast (ast : Ast.statement list) : llmodule =
  List.iter ~f:(fun stmt -> ignore (codegen_statement stmt)) ast;
  the_module

let codegen_ast_to_string (ast : Ast.statement list) : string =
  let module_ = codegen_ast ast in
  string_of_llmodule module_ *)