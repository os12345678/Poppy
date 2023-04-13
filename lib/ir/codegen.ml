open Poppy_parser
open Llvm
open Sexplib

(* Codegen Initialization *)

(* Codegen Initialization *)
let context = global_context ()
let builder = builder context
let the_module = create_module context "poppy_compiler"
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

(* Helper Functions *)
let counter = ref 0
(* generage_unique_id for lambda name generation *)
let generate_unique_id () =
  let time = Unix.gettimeofday () in
  let id = Printf.sprintf "%.0f_%d" time !counter in
  counter := !counter + 1;
  id
  let store_expr_in_global expr_value =
    let global_name = "global_" ^ (generate_unique_id ()) in
    let global_var = define_global global_name (const_null (type_of expr_value)) the_module in
    ignore (build_store expr_value global_var builder);
    global_var

let string_of_llmodule m =
  let s = string_of_llmodule m in
  dispose_module m;
  s

let print_module m =
  print_endline (string_of_llmodule m)
  
  

(* Codegen Expressions *)
let rec codegen_expr = function
  | Ast.IntLiteral i -> const_int (i64_type context) i
  | Ast.BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)
  | Ast.StringLiteral s -> const_stringz context s
  | Ast.Id s -> Hashtbl.find named_values s
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
      let args = Array.map codegen_expr args_array in
      build_call callee args "calltmp" builder
      | unimplement_expression ->
        let sexp = Ast.sexp_of_expr unimplement_expression in
        let expr_str = Sexp.to_string_hum sexp in
        raise (Failure ("expression not implemented: " ^ expr_str))
    
(* Codegen Prototype *)
let codegen_proto = function 
  | Ast.Prototype (Id name, args) -> 
    let doubles = Array.make (List.length args) (double_type context) in
    let ft = function_type (double_type context) doubles in
    let f = declare_function name ft the_module in
    let n = Array.length (params f) in
    if n == List.length args then () else
      raise (Failure "incorrect # arguments passed");
    for i = 0 to n - 1 do
      let Param (Id arg, _) = List.nth args i in
      let param = param f i in
      set_value_name arg param;
      Hashtbl.add named_values arg param
    done;
    f

(* Codegen Statement *)
let rec codegen_block = function
  | [] -> const_null (i32_type (global_context ())) (* return a null value for empty block *)
  | [last] -> codegen_statement last (* return the result of the last statement *)
  | stmt :: rest -> ignore (codegen_statement stmt); codegen_block rest
and codegen_statement = function
  | Ast.FuncDecl (proto, body) ->
    Hashtbl.clear named_values;
    let the_function = codegen_proto proto in
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    let ret_val = codegen_block body in
    let _ = build_ret ret_val builder in
    the_function
  | Ast.Expr expr -> codegen_expr expr
  | Ast.Return expr -> build_ret (codegen_expr expr) builder
  | unimplemented_statement -> 
    let sexp = Ast.sexp_of_statement unimplemented_statement in
    let stmt_str = Sexp.to_string_hum sexp in
    raise (Failure ("statement not implemented: " ^ stmt_str))



