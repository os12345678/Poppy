(* open Llvm
open Poppy_parser.Ast
open Scoping
open Sexplib
open Core

exception Codegen_error of string


(* Module and context creation *)
let context = Poppy_codegen__Codegen_util.context
let builder = Poppy_codegen__Codegen_util.builder
let the_module = create_module context "Poppy"
let named_values : (string, llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10

(* Utility functions for code generation *)
let rec codegen_expr (current_scope : scope) (expr : expr) (current_class_info : class_info) : Llvm.llvalue =
match expr with
| IntLiteral i -> const_int (i32_type context) i
| BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)
| StringType s -> build_global_stringptr s "str" builder
| StringLiteral s -> build_global_stringptr s "str" builder
| VoidType -> const_null (void_type context)
| Unit -> const_null (void_type context)

| Id id ->
  (match Poppy_codegen__Codegen_util.find_named_value id named_values with
   | Some value -> value
   | None -> failwith (Printf.sprintf "Unbound variable: %s" id))

| BinOp (op, e1, e2) ->
  let left = codegen_expr current_scope e1 current_class_info in
  let right = codegen_expr current_scope e2 current_class_info in
  Poppy_codegen__Codegen_util.codegen_binop op left right

| ClassInstantiation (var_name, class_name, exprs) ->
  (match find_class current_scope class_name with
  | Some class_info ->
    let instance = create_instance class_info in
    codegen_class_instance instance
  | None -> failwith (Printf.sprintf "Class not found: %s" class_name))
  
| ClassMemberAccess (instance_expr, member_name) ->
  let instance = codegen_expr current_scope instance_expr current_class_info in
  codegen_class_member_access instance member_name current_class_info

| Lambda (params, body) ->
  codegen_lambda current_scope params body current_class_info

| Call (func_name, args) ->
  codegen_call current_scope func_name args current_class_info

| This ->
  codegen_this current_scope current_class_info

| unimplemented_expression ->
  failwith (Printf.sprintf "Code generation not implemented for this expression: %s" (Sexp.to_string_hum (sexp_of_expr unimplemented_expression)))

  | unimplement_expression ->
    let sexp = sexp_of_expr unimplement_expression in
    let expr_str = Sexp.to_string_hum sexp in
    raise (Failure ("expression not implemented: " ^ expr_str)) *)
