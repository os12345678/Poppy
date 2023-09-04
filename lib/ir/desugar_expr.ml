open Core 
open Frontend_ir
open Poppy_parser

(* 
Flatten Nested Expressions: Convert complex expressions into a sequence of 
simpler expressions with temporary variables to hold intermediate results.

Desugar High-Level Constructs: Replace high-level constructs like for loops, 
let bindings, and method calls with simpler constructs.

Resolve Types: Ensure that all type information is resolved, replacing type 
variables with concrete types wherever possible.

Lower Functions and Methods: Convert all function and method definitions into 
a standard form.

Lower Control Flow: Convert control flow constructs like if-else and while into 
basic blocks and branch instructions. 
*)

module A = Poppy_parser.Ast
module T = Poppy_parser.Ast_types

let convert_bin_op op =
  match op with
  | T.BinOpPlus -> "+"
  | T.BinOpMinus -> "-"
  | T.BinOpMult -> "*"
  | T.BinOpIntDiv -> "/"
  | T.BinOpRem -> "%"
  | T.BinOpEq  -> "=="
  | T.BinOpNotEq  -> "!="
  | T.BinOpLessThan  -> "<"
  | T.BinOpLessThanEq  -> "<="
  | T.BinOpGreaterThan  -> ">"
  | T.BinOpGreaterThanEq  -> ">="
  | T.BinOpAnd -> "&&"
  | T.BinOpOr  -> "||"

let convert_un_op op =
  match op with
  | T.UnOpNot -> "!"
  | T.UnOpNeg -> "-"


(* The `new_temp` function is used to generate a new temporary variable name. 
   It appends the current value of the `counter` reference to the string "temp" 
   and increments the `counter` by 1. The generated temporary variable name is 
   then returned. *)
let counter = ref 0
let new_temp () =
  let temp = "temp" ^ (Int.to_string !counter) in
  counter := !counter + 1;
  temp

(* 
The `new_global_string` function is used to generate global string variables 
that is used for the printf wrapper function.    
*)

let global_strings = ref []

let new_global_string str =
  let var_name = "@" ^ (Int.to_string (List.length !global_strings)) in
  global_strings := !global_strings @ [var_name, str];
  LLVMVariable (var_name, Locked)

let convert_identifier id mutex_state =
  match id with
  | A.Variable var_name ->
    LLVMVariable (T.Var_name.to_string var_name, mutex_state)
  | A.ObjField (obj_name, field_name) ->
    LLVMField (T.Var_name.to_string obj_name, T.Field_name.to_string field_name, mutex_state)
  
let rec desugar_exprs (expr: Ast.expr) = 
  match expr.node with

  | A.Int i ->
    let temp = new_temp () in
    let llvm_expr = { llvm_loc = expr.loc; llvm_node = LLVMInt i } in
    ([{ llvm_loc = expr.loc; llvm_node = LLVMStore (LLVMVariable (temp, Locked), llvm_expr) }], 
     { llvm_loc = expr.loc; llvm_node = LLVMIdentifier (LLVMVariable (temp, Locked)) });
  
  | A.Boolean b -> 
    let temp = new_temp () in
    let llvm_expr = { llvm_loc = expr.loc; llvm_node = LLVMBool b } in
    ([{ llvm_loc = expr.loc; llvm_node = LLVMStore (LLVMVariable (temp, Locked), llvm_expr) }], 
     { llvm_loc = expr.loc; llvm_node = LLVMIdentifier (LLVMVariable (temp, Locked)) });

  | Identifier id ->
    let llvm_id = convert_identifier id Locked in 
    ([], { llvm_loc = expr.loc; llvm_node = LLVMIdentifier llvm_id });

  | A.Let (_, var_name, expr) ->
    let temp = T.Var_name.to_string var_name in
    let (stmts, llvm_expr) = desugar_exprs expr in
    (stmts @ [{ llvm_loc = expr.loc; llvm_node = LLVMStore (LLVMVariable (temp, Locked), llvm_expr) }], 
      { llvm_loc = expr.loc; llvm_node = LLVMIdentifier (LLVMVariable (temp, Locked)) });
    
  | A.Assign (id, expr) ->
    let llvm_id = convert_identifier id Locked in
    let (stmts, llvm_expr) = desugar_exprs expr in
    (stmts @ [{ llvm_loc = expr.loc; llvm_node = LLVMStore (llvm_id, llvm_expr) }], 
      { llvm_loc = expr.loc; llvm_node = LLVMIdentifier llvm_id });

  | A.Constructor (var_name, struct_name, constructor_args) ->
    let temp = T.Var_name.to_string var_name in
    let (stmts, llvm_exprs) = List.unzip (List.map ~f:desugar_constructor_arg constructor_args) in
    let stmts = List.concat stmts in
    let llvm_expr = { llvm_loc = expr.loc; llvm_node = LLVMCall (T.Struct_name.to_string struct_name, llvm_exprs) } in
    (stmts @ [{ llvm_loc = expr.loc; llvm_node = LLVMStore (LLVMVariable (temp, Locked), llvm_expr) }], 
      { llvm_loc = expr.loc; llvm_node = LLVMIdentifier (LLVMVariable (temp, Locked)) });

  | A.MethodApp (obj_name, method_name, args) ->
    let (stmts, llvm_args) = List.unzip (List.map ~f:desugar_exprs args) in
    let stmts = List.concat stmts in
    let method_name = T.Var_name.to_string obj_name ^ "_" ^ T.Method_name.to_string method_name in
    (stmts, { llvm_loc = expr.loc; llvm_node = LLVMCall (method_name, llvm_args) })
      
  | A.FunctionApp (func_name, args) ->
    let (stmts, llvm_args) = List.unzip (List.map ~f:desugar_exprs args) in
    let stmts = List.concat stmts in
    let func_name = T.Function_name.to_string func_name in
    (stmts, { llvm_loc = expr.loc; llvm_node = LLVMCall (func_name, llvm_args) })
      
  | A.If (cond, then_block, else_block) ->
    let (cond_stmts, llvm_cond) = desugar_exprs cond in
    let (then_stmts, llvm_then_block) = desugar_block_expr then_block in
    let (else_stmts, llvm_else_block) = desugar_block_expr else_block in
    (cond_stmts @ then_stmts @ else_stmts, 
      { llvm_loc = expr.loc; llvm_node = LLVMIf (llvm_cond, llvm_then_block, llvm_else_block) })
    
  | A.While (cond, block) ->
    let (cond_stmts, llvm_cond) = desugar_exprs cond in
    let (block_stmts, llvm_block) = desugar_block_expr block in
    (cond_stmts @ block_stmts, 
      { llvm_loc = expr.loc; llvm_node = LLVMWhile (llvm_cond, llvm_block) })
      
  | A.For (init, cond, update, block) ->
    let tmp = new_temp () in
    let (init_stmts, llvm_init) = desugar_exprs init in
    let (cond_stmts, llvm_cond) = desugar_exprs cond in
    let (update_stmts, llvm_update) = desugar_exprs update in
    let (block_stmts, llvm_block) = desugar_block_expr block in
    (init_stmts @ cond_stmts @ update_stmts @ block_stmts @ 
      [{ llvm_loc = expr.loc; llvm_node = LLVMBinOp ("=", llvm_init, llvm_cond) };
      { llvm_loc = expr.loc; llvm_node = LLVMStore (LLVMVariable (tmp, Locked), llvm_cond) };
      { llvm_loc = expr.loc; llvm_node = LLVMWhile (llvm_cond, llvm_block) };
      { llvm_loc = expr.loc; llvm_node = LLVMBinOp ("=", llvm_update, llvm_cond) }],
      { llvm_loc = expr.loc; llvm_node = LLVMIdentifier (LLVMVariable (tmp, Locked)) })

  | A.Printf (format_str, exprs) ->
    let (stmts, llvm_exprs) = List.unzip (List.map ~f:desugar_exprs exprs) in
    let stmts = List.concat stmts in
    let format_str_var = new_global_string format_str in
    let llvm_expr_nodes = List.map ~f:(fun e -> e.llvm_node) llvm_exprs in
    let llvm_expr_nodes = List.map ~f:(fun e -> { llvm_loc = expr.loc; llvm_node = e }) (LLVMIdentifier format_str_var :: llvm_expr_nodes) in
    let llvm_expr = { llvm_loc = expr.loc; llvm_node = LLVMCall ("printf", llvm_expr_nodes) } in
    (stmts, llvm_expr)
      
  | A.BinOp (op, lhs, rhs) ->
    let (lhs_stmts, llvm_lhs) = desugar_exprs lhs in
    let (rhs_stmts, llvm_rhs) = desugar_exprs rhs in
    let llvm_op = convert_bin_op op in
    (lhs_stmts @ rhs_stmts, { llvm_loc = expr.loc; llvm_node = LLVMBinOp (llvm_op, llvm_lhs, llvm_rhs) })
  
  | A.UnOp (op, expr) ->
    let (expr_stmts, llvm_expr) = desugar_exprs expr in
    let llvm_op = convert_un_op op in
    (expr_stmts, { llvm_loc = expr.loc; llvm_node = LLVMUnOp (llvm_op, llvm_expr) })

  | _ -> failwith "Not implemented"
      
and desugar_constructor_arg (ConstructorArg (_, expr)) =
  let (stmts, llvm_expr) = desugar_exprs expr in
  (stmts, llvm_expr)

and desugar_block_expr (block: A.block_expr) =
  match block with
  | A.Block (loc, exprs) ->
    let (stmts, llvm_exprs) = List.unzip (List.map ~f:desugar_exprs exprs) in
    let stmts = List.concat stmts in
    (stmts, LLVMBlock (loc, llvm_exprs))
  