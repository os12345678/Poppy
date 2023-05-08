open Llvm
open Poppy_parser.Ast
open Scoping
(* open Sexplib *)
open Core
(* open Poppy_type_checker *)
open Poppy_codegen__Codegen_util

exception Codegen_error of string


(* Module and context creation *)
let context = context
let builder = builder
let the_module = the_module
let named_values : (string, llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10
let function_protos: (string, (string * func_param list * type_decl) option) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 50
let class_protos: (string, class_info) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 50

(* Utility functions for code generation *)
let rec codegen_expr (expr: expr) (scope: scope) (class_info: class_info) (builder: Llvm.llbuilder) (func_map: (string, Llvm.llvalue) Stdlib.Hashtbl.t): Llvm.llvalue =
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
  let left = codegen_expr e1 scope class_info builder func_map in
  let right = codegen_expr e2 scope class_info builder func_map in
  codegen_binop op left right

| Call (func_name, args) ->
  let llvm_args = List.map ~f:(fun arg -> codegen_expr arg scope class_info builder func_map) args in
  codegen_call func_name llvm_args func_map
  
(*
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
  codegen_this current_scope current_class_info *)

  | unimplement_expression ->
    let sexp = sexp_of_expr unimplement_expression in
    let expr_str = Sexp.to_string_hum sexp in
    raise (Failure ("expression not implemented: " ^ expr_str))

