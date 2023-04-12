open Poppy_parser.Ast
open Poppy_parser.Parser
open Llvm
open Llvm_executionengine
open Llvm_target

let context = global_context ()
let builder = builder context
let the_module = create_module context "poppy_compiler"
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let rec codegen_expr = function
  | Poppy_parser.Ast.IntLiteral i -> const_int (i64_type context) i
  | Poppy_parser.Ast.BoolLiteral b -> const_int (i1_type context) (if b then 1 else 0)
  | Poppy_parser.Ast.StringLiteral s -> const_stringz context s
  | Poppy_parser.Ast.Id s -> Hashtbl.find named_values s
  | Poppy_parser.Ast.BinOp (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin 
      match op with
      | Poppy_parser.Ast.Plus -> build_add lhs_val rhs_val "addtmp" builder
      | Poppy_parser.Ast.Minus -> build_sub lhs_val rhs_val "subtmp" builder
      | Poppy_parser.Ast.Times -> build_mul lhs_val rhs_val "multmp" builder
      | Poppy_parser.Ast.Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      | Poppy_parser.Ast.Lt -> 
        let i = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Gt ->
        let i = build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Leq ->
        let i = build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Geq ->
        let i = build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Eq ->
        let i = build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Neq ->
        let i = build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.And ->
        let i = build_and lhs_val rhs_val "andtmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Or ->
        let i = build_or lhs_val rhs_val "ortmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | Poppy_parser.Ast.Xor ->
        let i = build_xor lhs_val rhs_val "xortmp" builder in
        build_zext i (i1_type context) "booltmp" builder
      | _ -> raise (Failure "invalid binary operator")
    end
    | Poppy_parser.Ast.Not e ->
      let e_val = codegen_expr e in
      let i = build_not e_val "nottmp" builder in
      build_zext i (i1_type context) "booltmp" builder
    | Poppy_parser.Ast.Incr i -> raise (Failure "not implemented")
    | Poppy_parser.Ast.Decr d -> raise (Failure "not implemented")
    | Poppy_parser.Ast.Lambda (args, body) -> raise (Failure "not implemented")
    | Poppy_parser.Ast.Call (callee, args) ->
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
      | _ -> raise (Failure "not implemented")

    


let double_type = double_type context


