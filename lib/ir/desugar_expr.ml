open Core 
open Desugared_ast

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

module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types

let extract_var_name (id: T.typed_identifier) : string = 
  match id with
  | T.TVariable (name, _) -> A.Var_name.to_string name
  | T.TObjField (name, _, _) -> A.Var_name.to_string name
  
type function_call = {
  fname: string;
  args: dexpr list;
}
  
let counter = ref 0
let rec desugar_expr (te: T.expr) : Desugared_ast.dexpr = 
  match te.node with
  | TInt i -> 
    { loc = te.loc; typ = TEInt; node = DIntLit i }
  | TBoolean b -> 
    { loc = te.loc; typ = TEBool; node = DBoolLit b }

  (* Variables and Assignments *)
  | TIdentifier id -> 
    let var_name = extract_var_name id in 
    { loc = te.loc; 
      typ = te.typ; 
      node = DVar var_name }
  | TAssign (id, expr) -> 
    let var_name = extract_var_name id in
    let desugared_expr = desugar_expr expr in
    { loc = te.loc; 
      typ = te.typ; 
      node = DAssign (var_name, desugared_expr.node) }
  
  (* Binary/unary Operators *)
  | TBinOp (op, e1, e2) -> 
    let de1 = desugar_expr e1 in
    let de2 = desugar_expr e2 in
    { loc = te.loc; 
      typ = te.typ; 
      node = DBinOp (op, de1.node, de2.node) }
  | TUnOp (op, e) -> 
    let de = desugar_expr e in
    { loc = te.loc; 
      typ = te.typ; 
      node = DUnOp (op, de.node) }

  (* Application *)
  | TFunctionApp (fname, args) -> 
    let desugared_args = List.map ~f:desugar_expr args in
    { loc = te.loc; 
      typ = te.typ; 
      node = DCall (A.Function_name.to_string fname, List.map ~f:(fun e -> e.node) desugared_args) }
  | TPrintf (format_str, exprs) ->
    let desugared_exprs = List.map ~f:desugar_expr exprs in
    let desugared_args = List.map ~f:(fun de -> de.node) desugared_exprs in
    {
      loc = te.loc;
      typ = TEVoid; (* printf returns void *)
      node = DCall ("print", [DStringLit format_str] @ desugared_args);
    }

  (* Control Flow *)
  | TIf (cond, then_block, else_block) -> 
    let dcond = desugar_expr cond in
    let dthen = desugar_block then_block in
    let delse = desugar_block else_block in
    { loc = te.loc; 
      typ = te.typ; 
      node = DIf (dcond.node, dthen, delse) }
  | TWhile (cond, block) -> 
    let dcond = desugar_expr cond in
    let dblock = desugar_block block in
    { loc = te.loc; 
      typ = TEVoid; 
      node = DWhile (dcond.node, dblock) }

  (* Async Constructs *)
  | TFinishAsync (asyncs, block) -> 
    (* Extract all function calls from each async block *)
    let all_calls = List.concat_map ~f:extract_calls_from_async asyncs in
    (* Create a thread for each function call *)
    let thread_creations = List.map ~f:(fun call -> 
      DCreateThread (call.fname, List.map ~f:(fun de -> de.node) call.args)
    ) all_calls in
    (* Join all threads after they've been created *)
    let thread_joins = List.map ~f:(fun call -> DJoinThread (DVar call.fname)) all_calls in
    let dblock = desugar_block block in
    (* Combine thread creations, joins, and the main block *)
    let dblock_nodes = List.map ~f:(fun expr -> expr) dblock in
    let combined_nodes = thread_creations @ thread_joins @ dblock_nodes in
    { loc = te.loc; 
      typ = TEVoid; 
      node = DBlockExpr combined_nodes }
    
  | _ -> failwith "Not implemented"
     

and desugar_block (tb: T.block_expr) : Desugared_ast.dblock =
(* increment the counter by 1 each time desugar block is called *)
counter := !counter + 1;
print_endline ("\ndesugar block called " ^ (string_of_int !counter) ^ " times");
match tb with
| Block (_, _, exprs) ->
  List.concat_map ~f:(fun te ->
    let de = desugar_expr te in
    match te.node with
    | TBlockExpr inner_block -> 
        desugar_block inner_block (* Flatten nested blocks *)
    | _ -> [de.node]
  ) exprs

and extract_calls_from_async (async: T.async_expr) : function_call list =
  match async with
  | AsyncExpr (Block (_, _, exprs)) -> 
    List.filter_map ~f:(fun expr ->
      match expr.node with
      | TFunctionApp (fname, args) -> 
        let desugared_args = List.map ~f:desugar_expr args in
        Some { fname = A.Function_name.to_string fname; args = desugared_args }
      | _ -> None
    ) exprs