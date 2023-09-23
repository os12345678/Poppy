open Llvm
open Desugar.Desugared_ast

exception Codegen_error of string

module D = Desugar.Desugared_ast
module A = Poppy_parser.Ast_types

(* Symbol table setup and helper functions *)

(* 
Note: symbol table information is optional. Leaving a placeholder such as None 
build a symbol table with placeholders for the LLVM information (e.g., setting 
llvm_value and llvm_type to None), and then populate those fields during the 
code generation phase.
*)

module Symbol = struct
  type t = string
  let compare = compare
end

module SymbolMap = Map.Make(Symbol)

type storage_type = 
  | Local
  | Global

type llvm_symbol_info = 
  | LVarInfo of {
    llvm_value : llvalue option;
    llvm_type  : lltype option;
    storage    : storage_type;
    is_global  : bool;
  }
  | LFuncInfo of {
    llvm_function : llvalue option;
    params        : llvm_symbol_info list;
  }
  | LStructInfo of {
    llvm_struct: llvalue option;
    field_map : (string, int) Hashtbl.t; (* Maps field names to their indices in the struct *)
  }

  type llvm_symbol_table = {
    table : llvm_symbol_info SymbolMap.t;
    parent: llvm_symbol_table option;
  }

let current_sym_table = ref { table = SymbolMap.empty; parent = None }
let sym_table_stack : llvm_symbol_table Stack.t = Stack.create ()

let enter_scope current_sym_table_ref =
  Stack.push !current_sym_table_ref sym_table_stack;
  current_sym_table_ref := { table = SymbolMap.empty; parent = Some !current_sym_table_ref }

let exit_scope current_sym_table_ref =
  try
    current_sym_table_ref := Stack.pop sym_table_stack
  with Stack.Empty ->
    failwith "Trying to exit global scope, this should not happen."

let add_symbol (sym_table_ref: llvm_symbol_table ref) (name: string) (info: llvm_symbol_info) =
  let current_table = !sym_table_ref in
  let new_table = SymbolMap.add name info current_table.table in
  sym_table_ref := { current_table with table = new_table }
    
let rec lookup_variable (sym_table_ref: llvm_symbol_table ref) (name: string) : llvm_symbol_info option =
  let sym_table = !sym_table_ref in
  match SymbolMap.find_opt name sym_table.table with
  | Some info -> Some info (* Found in the current scope *)
  | None ->  (* Not found in the current scope, look in the parent scope *)
      match sym_table.parent with
      | Some parent_table -> lookup_variable (ref parent_table) name
      | None -> None  (* Reached global scope, symbol not found *)
  
  (* TODO *)
let process_structs (sym_table_ref: llvm_symbol_table ref) (structs: dstruct list) : unit =
  List.iter (fun dstruct ->
    let struct_name = dstruct.name in
    (* Create a new empty field map *)
    let field_map = Hashtbl.create (List.length dstruct.fields) in
    List.iteri (fun idx (field_name, _) -> Hashtbl.add field_map field_name idx) dstruct.fields;
    let struct_info = LStructInfo { llvm_struct = None; field_map = field_map } in
    add_symbol sym_table_ref struct_name struct_info
  ) structs

  (* TODO *)
let process_functions (sym_table_ref: llvm_symbol_table ref) (functions: dfunction list) : unit =
  List.iter (fun dfunction ->
    let params = List.map (fun param ->
      let _param_name, _param_type = param in 
      LVarInfo {
        llvm_value = None; 
        llvm_type = None; 
        storage = Local; 
        is_global = false;
      }
    ) dfunction.params in
    let func_name = dfunction.name in
    let func_info = LFuncInfo { llvm_function = None; params = params } in
    add_symbol sym_table_ref func_name func_info
  ) functions  

let rec process_expr (sym_table_ref: llvm_symbol_table ref) (expr: dexpr) : unit =
  match expr.node with
  | DIntLit _ | DBoolLit _ | DStringLit _ -> ()
  | DBlockExpr nodes -> 
    enter_scope sym_table_ref;
    List.iter (fun node ->
        let wrapped_expr = { 
            loc = expr.loc;
            typ = expr.typ;
            node = node 
        } in
        process_expr sym_table_ref wrapped_expr
    ) nodes;
    exit_scope sym_table_ref
  | DVar name -> 
    let var_info = LVarInfo { llvm_value = None; llvm_type = None; storage = Local; is_global = false } in
    add_symbol sym_table_ref name var_info
  | DAssign (name, e) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
    process_expr sym_table_ref wrapped_expr;
    let var_info = lookup_variable sym_table_ref name in 
    begin 
      match var_info with 
      | Some (LVarInfo _) ->
        (* The variable already exists in the symbol table, and will be updated in the code generation phase *)
        ()
      | None ->
        (* Variable not declared, add it to the symbol table with placeholders *)
        let new_var_info = LVarInfo { llvm_value = None; llvm_type = None; storage = Local; is_global = false } in
        add_symbol sym_table_ref name new_var_info
      | Some (LFuncInfo _ | LStructInfo _) ->
        (* Mismatched identifier type, which ideally should not happen if type-checking phase is correct *)
        failwith (Printf.sprintf "Identifier %s is not a variable, but type-checking phase passed!!!" name)
    end
    
  | DBinOp (_, e1, e2) ->
    let wrapped_expr1 = { loc = expr.loc; typ = expr.typ; node = e1 } in
    let wrapped_expr2 = { loc = expr.loc; typ = expr.typ; node = e2 } in
    process_expr sym_table_ref wrapped_expr1;
    process_expr sym_table_ref wrapped_expr2
  | DUnOp (_, e) ->
      let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
      process_expr sym_table_ref wrapped_expr
  | DCall (fname, args) ->
    (* Process each argument *)
    List.iter (fun arg ->
        let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = arg } in
        process_expr sym_table_ref wrapped_expr
    ) args;
    (* Look up the function name in the symbol table *)
    let func_info_option = lookup_variable sym_table_ref fname in
    begin 
      match func_info_option with
      | Some (LFuncInfo _) ->
          (* The function already exists in the symbol table, and will be filled in during the code generation phase *)
          ()
      | None ->
          (* Function not declared, add it to the symbol table with placeholders *)
          let new_func_info = LFuncInfo { llvm_function = None; params = [] } in
          add_symbol sym_table_ref fname new_func_info
      | Some (LVarInfo _ | LStructInfo _) ->
          (* Mismatched identifier type, which should not happen if the type-checking phase is correct *)
          failwith (Printf.sprintf "Identifier %s is not a function, but type-checking phase passed!!!" fname)
      end
  | DIf (cond, then_block, else_block) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = cond } in
    process_expr sym_table_ref wrapped_expr;
    process_block sym_table_ref then_block;
    process_block sym_table_ref else_block
  | DWhile (cond, block) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = cond } in
    process_expr sym_table_ref wrapped_expr;
    process_block sym_table_ref block
  | DCreateThread (fname, args) ->
    (* Process each argument *)
    List.iter (fun arg ->
        let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = arg } in
        process_expr sym_table_ref wrapped_expr
    ) args;
    (* Look up the function name in the symbol table *)
    let func_info_option = lookup_variable sym_table_ref fname in
    begin 
      match func_info_option with
      | Some (LFuncInfo _) ->
          (* The function already exists in the symbol table; assume it's valid for threading *)
          ()
      | None ->
          (* Function not declared, add it to the symbol table with placeholders *)
          let new_func_info = LFuncInfo { llvm_function = None; params = [] } in
          add_symbol sym_table_ref fname new_func_info
      | Some (LVarInfo _ | LStructInfo _) ->
          (* Mismatched identifier type, which should not happen if the type-checking phase is correct *)
          failwith (Printf.sprintf "Identifier %s is not a function, but type-checking phase passed!!!" fname)
      end
  | DJoinThread e ->
      let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
      process_expr sym_table_ref wrapped_expr

and process_block sym_table_ref block =
  enter_scope sym_table_ref;
  List.iter (fun expr -> process_expr sym_table_ref expr) block;
  exit_scope sym_table_ref

let build_symbol_table program =
  let sym_table_ref = ref { table = SymbolMap.empty; parent = None } in
  enter_scope sym_table_ref;
  process_structs sym_table_ref program.structs;
  process_functions sym_table_ref program.functions;
  process_block sym_table_ref program.main;
  exit_scope sym_table_ref;
  !sym_table_ref