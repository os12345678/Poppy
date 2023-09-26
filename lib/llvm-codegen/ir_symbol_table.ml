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

let sym_table_stack : llvm_symbol_table Stack.t = Stack.create ()

let enter_scope (current_sym_table: llvm_symbol_table) : llvm_symbol_table =
  let new_scope = { table = SymbolMap.empty; parent = Some current_sym_table } in
  Stack.push new_scope sym_table_stack;
  new_scope

let exit_scope (current_sym_table: llvm_symbol_table) : llvm_symbol_table =
  match current_sym_table.parent with
  | Some parent_table -> 
      (* Merge current table into parent *)
      let merged_table = SymbolMap.union (fun _ val1 _ -> Some val1) current_sym_table.table parent_table.table in
      let merged_sym_table = { parent_table with table = merged_table } in
      ignore (Stack.pop sym_table_stack);  (* Pop the current scope off the stack *)
      merged_sym_table
  | None -> 
      failwith "Trying to exit global scope, this should not happen."
  
let add_symbol (sym_table: llvm_symbol_table) (name: string) (info: llvm_symbol_info) : llvm_symbol_table =
  let new_table = SymbolMap.add name info sym_table.table in
  let updated_sym_table = { sym_table with table = new_table } in
  updated_sym_table

let rec lookup_variable (sym_table: llvm_symbol_table) (name: string) : llvm_symbol_info option =
  match SymbolMap.find_opt name sym_table.table with
  | Some info -> Some info (* Found in the current scope *)
  | None ->  (* Not found in the current scope, look in the parent scope *)
      match sym_table.parent with
      | Some parent_table -> lookup_variable parent_table name
      | None -> None  (* Reached global scope, symbol not found *)

let rec print_symbol_table (sym_table: llvm_symbol_table) : unit =
  Printf.printf "=== Symbol Table ===\n";
  SymbolMap.iter (fun key symbol_info ->
    Printf.printf "Symbol Name: %s\n" key;
    print_symbol_info symbol_info
  ) sym_table.table;
  match sym_table.parent with
  | Some parent_table -> 
      Printf.printf "\n--- Parent Table ---\n";
      print_symbol_table parent_table
  | None -> 
      Printf.printf "\n=== End of Symbol Table ===\n"

and print_symbol_info info =
  match info with
  | LVarInfo { llvm_value; llvm_type; storage; is_global } ->
      Printf.printf "\tType: Variable\n";
      Printf.printf "\tLLVM Value: %s\n" (Option.value ~default:"None" (Option.map string_of_llvalue llvm_value));
      Printf.printf "\tLLVM Type: %s\n" (Option.value ~default:"None" (Option.map string_of_lltype llvm_type));
      Printf.printf "\tStorage: %s\n" (match storage with | Local -> "Local" | Global -> "Global");
      Printf.printf "\tIs Global: %b\n" is_global;
  | LFuncInfo { llvm_function; params } ->
      Printf.printf "\tType: Function\n";
      Printf.printf "\tLLVM Function: %s\n" (Option.value ~default:"None" (Option.map string_of_llvalue llvm_function));
      Printf.printf "\tParams:\n";
      List.iter print_symbol_info params
  | LStructInfo { llvm_struct; field_map } ->
      Printf.printf "\tType: Struct\n";
      Printf.printf "\tLLVM Struct: %s\n" (Option.value ~default:"None" (Option.map string_of_llvalue llvm_struct));
      Printf.printf "\tFields:\n";
      Hashtbl.iter (fun field_name index -> Printf.printf "\t\t%s : %d\n" field_name index) field_map
      

  (* TODO *)
let process_structs (sym_table: llvm_symbol_table) (structs: dstruct list) : llvm_symbol_table =
  List.fold_left (fun current_table dstruct ->
    let struct_name = dstruct.name in
    (* Create a new empty field map *)
    let field_map = Hashtbl.create (List.length dstruct.fields) in
    List.iteri (fun idx (field_name, _) -> Hashtbl.add field_map field_name idx) dstruct.fields;
    let struct_info = LStructInfo { llvm_struct = None; field_map = field_map } in
    add_symbol current_table struct_name struct_info
  ) sym_table structs

  (* TODO *) 
  let process_functions (sym_table: llvm_symbol_table) (functions: dfunction list) : llvm_symbol_table =
  List.fold_left (fun current_table dfunction ->
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
    add_symbol current_table func_name func_info
  ) sym_table functions
  


let rec process_expr (sym_table: llvm_symbol_table) (expr: dexpr) : llvm_symbol_table =
  match expr.node with
  | DIntLit _ | DBoolLit _ | DStringLit _ -> sym_table
  | DBlockExpr nodes -> 
    let new_sym_table = enter_scope sym_table in
    let final_sym_table = List.fold_left (fun acc_sym_table node ->
        let wrapped_expr = { 
            loc = expr.loc;
            typ = expr.typ;
            node = node 
        } in
        process_expr acc_sym_table wrapped_expr
    ) new_sym_table nodes in
    exit_scope final_sym_table
  | DVar name -> 
    let var_info = LVarInfo { llvm_value = None; llvm_type = None; storage = Local; is_global = false } in
    add_symbol sym_table name var_info
  | DAssign (name, e) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
    let updated_sym_table = process_expr sym_table wrapped_expr in
    let var_info = lookup_variable sym_table name in 
    begin 
      match var_info with 
      | Some (LVarInfo _) ->
        updated_sym_table
      | None ->
        let new_var_info = LVarInfo { llvm_value = None; llvm_type = None; storage = Local; is_global = false } in
        add_symbol updated_sym_table name new_var_info
      | Some (LFuncInfo _ | LStructInfo _) ->
        failwith (Printf.sprintf "Identifier %s is not a variable, but type-checking phase passed!!!" name)
    end
  | DBinOp (_, e1, e2) ->
    let wrapped_expr1 = { loc = expr.loc; typ = expr.typ; node = e1 } in
    let wrapped_expr2 = { loc = expr.loc; typ = expr.typ; node = e2 } in
    let updated_sym_table_lfs = process_expr sym_table wrapped_expr1 in
    process_expr updated_sym_table_lfs wrapped_expr2

  | DUnOp (_, e) ->
      let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
      process_expr sym_table wrapped_expr
  | DCall (fname, args) ->
    let updated_sym_table = List.fold_left (fun current_sym_table arg ->
        let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = arg } in
        process_expr current_sym_table wrapped_expr
    ) sym_table args in
    let func_info_option = lookup_variable updated_sym_table fname in
    begin 
      match func_info_option with
      | Some (LFuncInfo _) ->
          updated_sym_table
      | None ->
          let new_func_info = LFuncInfo { llvm_function = None; params = [] } in
          add_symbol updated_sym_table fname new_func_info
      | Some (LVarInfo _ | LStructInfo _) ->
          failwith (Printf.sprintf "Identifier %s is not a function, but type-checking phase passed!!!" fname)
    end
    
  | DIf (cond, then_block, else_block) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = cond } in
    let updated_sym_table1 = process_expr sym_table wrapped_expr in
    let updated_sym_table2 = process_block updated_sym_table1 then_block in
    process_block updated_sym_table2 else_block
  
  | DWhile (cond, block) ->
      let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = cond } in
      let updated_sym_table1 = process_expr sym_table wrapped_expr in
      process_block updated_sym_table1 block
  
  | DCreateThread (fname, args) ->
    let updated_sym_table = List.fold_left (fun acc_sym_table arg ->
        let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = arg } in
        process_expr acc_sym_table wrapped_expr
    ) sym_table args in

    (* Look up the function name in the updated symbol table *)
    let func_info_option = lookup_variable updated_sym_table fname in
    begin 
      match func_info_option with
      | Some (LFuncInfo _) ->
          updated_sym_table  (* The function already exists in the symbol table; assume it's valid for threading *)
      | None ->
          (* Function not declared, add it to the symbol table with placeholders *)
          let new_func_info = LFuncInfo { llvm_function = None; params = [] } in
          add_symbol updated_sym_table fname new_func_info
      | Some (LVarInfo _ | LStructInfo _) ->
          (* Mismatched identifier type, which should not happen if the type-checking phase is correct *)
          failwith (Printf.sprintf "Identifier %s is not a function, but type-checking phase passed!!!" fname)
    end

| DJoinThread e ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
    process_expr sym_table wrapped_expr
    
and process_block sym_table block =
  let entered_sym_table = enter_scope sym_table in
  let final_sym_table = List.fold_left (fun acc_sym_table expr -> 
      process_expr acc_sym_table expr
  ) entered_sym_table block in
  (* final_sym_table *)
  exit_scope final_sym_table

and build_symbol_table program =
  let sym_table = { table = SymbolMap.empty; parent = None } in
  let sym_table_after_entering_scope = enter_scope sym_table in
  let sym_table_after_structs = process_structs sym_table_after_entering_scope program.structs in
  let sym_table_after_functions = process_functions sym_table_after_structs program.functions in
  let sym_table_after_main = process_block sym_table_after_functions program.main in
  (* sym_table_after_main *)
  exit_scope sym_table_after_main
    


