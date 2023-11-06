open Core 
open Desugared_ast
(* open Poppy_type_checker *)

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
module E = Desugar_env

let generate_thread_id index = "thread_" ^ (string_of_int index)

let extract_var_name (id: T.typed_identifier) : string = 
  match id with
  | T.TVariable (name, _, _, _) -> A.Var_name.to_string name
  | T.TObjField (_, obj_name, _, field_name, _, _) -> 
    A.Var_name.to_string obj_name ^ "." ^ A.Field_name.to_string field_name

type function_call = {
  fname: string;
  args: dexpr list;
}

let thread_counters = Core.Hashtbl.Poly.create () 

let get_next_thread_name fname =
  let count = 
    match Core.Hashtbl.find thread_counters fname with
    | Some n -> n
    | None -> 0
  in
  Core.Hashtbl.set thread_counters ~key:fname ~data:(count + 1);
  if count = 0 then "thread_id_"^fname else Printf.sprintf "thread_id_%s%d" fname count


let rec desugar_expr (te: T.expr): Desugared_ast.dexpr = 
  match te.node with
  | TInt i -> 
    { loc = te.loc; typ = TEInt; node = DIntLit i }
  | TBoolean b -> 
    { loc = te.loc; typ = TEBool; node = DBoolLit b }
  | TBlockExpr block -> 
    let desugared_block = desugar_block block in
    let block_nodes = List.map ~f:(fun expr -> expr.node) desugared_block in
    {
      loc = te.loc;
      typ = te.typ;
      node = DBlockExpr block_nodes;
    }

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
  | TLet (_, var_name, expr) ->
    let desugared_expr = desugar_expr expr in
    { loc = te.loc; typ = te.typ; node = DAssign (A.Var_name.to_string var_name, desugared_expr.node) }
  | TConstructor (var_name, struct_name, constructor_args) ->
    (* Convert constructor_args into a list of dexpr_nodes *)
    let arg_nodes = List.map ~f:(fun (ConstructorArg (_, expr)) ->
        desugar_expr expr
    ) constructor_args in
    (* Extract the node field from each dexpr to get a list of dexpr_nodes *)
    let arg_node_values = List.map ~f:(fun de -> de.node) arg_nodes in
    (* Create a special constructor function name *)
    let constructor_fn_name = "init_" ^ (A.Struct_name.to_string struct_name) in
    (* Create the DCall node for the constructor function *)
    let call_node = { loc = te.loc; typ = te.typ; node = DCall (constructor_fn_name, arg_node_values) } in
    (* Create the DAssign node to assign the result of the constructor call to the variable *)
    { loc = te.loc; 
      typ = te.typ; 
      node = DAssign (A.Var_name.to_string var_name, call_node.node) }
  
  
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
    let desugared_args = List.map ~f:(fun args -> desugar_expr args ) args in
    { loc = te.loc; 
      typ = te.typ; 
      node = DCall (A.Function_name.to_string fname, List.map ~f:(fun e -> e.node) desugared_args) }

  | TMethodApp (obj_name, struct_name, trait_name, method_name, _, args) ->
    let obj_expr = { loc = te.loc; typ = te.typ; node = DVar (A.Var_name.to_string obj_name) } in
    let desugared_args = List.map ~f:(fun args -> desugar_expr args ) args in
    let mangled_fn_name = 
      E.mangle_impl trait_name struct_name method_name in
    { loc = te.loc; 
      typ = te.typ; 
      node = DCall (mangled_fn_name, obj_expr.node :: (List.map ~f:(fun de -> de.node) desugared_args)) }
    
  | TPrintf (format_str, exprs) ->
    let desugared_exprs = List.map ~f:(fun exprs -> desugar_expr exprs ) exprs in
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

  (*   | TFinishAsync (asyncs, obj_var_and_capabilities, block) -> 
    print_endline "Desugaring finish_async";

    (* Step 2: Desugar async expressions into thread creations *)
    let thread_creations = List.mapi asyncs ~f:(fun idx async_expr ->
        let thread_func_name = "thread_func_" ^ Int.to_string idx in
        (* Assume desugar_async_expr generates a function from an async expression *)
        let thread_func = desugar_async_expr ~shared_vars:obj_var_and_capabilities async_expr in
        DCreateThread (thread_func_name, thread_func)
    ) in

    (* Step 3: Desugar the main block expression *)
    let dblock = desugar_block block in

    (* Step 4: Generate thread join expressions *)
    let thread_joins = List.mapi asyncs ~f:(fun idx _ ->
        let thread_func_name = "thread_func_" ^ Int.to_string idx in
        DJoinThread (DVar (thread_func_name))
    ) in

    (* Combine thread creations, joins, and the main block *)
    let dblock_nodes = List.map ~f:(fun expr -> expr.node) dblock in
    let combined_nodes = thread_creations @ thread_joins @ dblock_nodes in

    { loc = te.loc; 
      typ = TEVoid; 
      node = DBlockExpr combined_nodes }
 *)

  | TFinishAsync (asyncs, _obj_var_and_capabilities, block) -> 
    print_endline "Desugaring finish_async";
    (* Extract all function calls from each async block *)
    let all_desugared_exprs = List.concat_map ~f:desugar_async_exprs asyncs in
    print_endline ("Desugared expressions: " ^ (string_of_int (List.length all_desugared_exprs)));
    let thread_creations = List.mapi ~f:(fun index expr -> 
      let thread_id = generate_thread_id index in
      DCreateThread (thread_id, [expr.node])
    ) all_desugared_exprs in
    print_endline ("Thread creates: " ^ (string_of_int (List.length thread_creations)));

    (* Join all threads after they've been created *)
    let thread_joins = List.mapi ~f:(fun index _expr -> 
      let thread_id = generate_thread_id index in
      DJoinThread (DVar (thread_id))
    ) all_desugared_exprs in
    print_endline ("Thread joins: " ^ (string_of_int (List.length thread_joins)));

    let dblock = desugar_block block in
    let dblock_nodes = List.map ~f:(fun expr -> expr.node) dblock in
    let combined_nodes = thread_creations @ thread_joins @ dblock_nodes in
    { loc = te.loc; 
      typ = TEVoid; 
      node = DBlockExpr combined_nodes }

  | _ -> failwith "Desugar: Consume not implemented"
         
and desugar_block (tb: T.block_expr) : Desugared_ast.dblock =
  match tb with
  | Block (_, _, exprs) ->
    let results = List.concat_map ~f:(fun te ->
      let de = desugar_expr te in
      match te.node with
      | TBlockExpr inner_block -> 
          let inner = desugar_block inner_block in
          inner
      | _ -> [de]
    ) exprs in
    results
      
and desugar_async_exprs (async: T.async_expr) : dexpr list =
  match async with
  | AsyncExpr (_, (Block (_, _, exprs))) -> 
    print_endline "found async expr block";
    List.map ~f:desugar_expr exprs
  