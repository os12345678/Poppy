open Llvm
(* open Codegen_util *)
(* open Desugar.Desugared_ast *)

exception Codegen_error of string

module L = Llvm
module D = Desugar.Desugared_ast
module U = Codegen_util
module T = Poppy_parser.Ast_types
module St = Ir_symbol_table
module E = Link_extern

let wrap loc typ node = {
  D.loc = loc;
  D.typ = typ;
  D.node = node;
}

let llvm_type_of_typ = function
  | T.TEInt -> Llvm.i32_type U.context
  | T.TEBool -> Llvm.i1_type U.context
  | T.TEVoid -> Llvm.void_type U.context
  | T.TEStruct s -> Llvm.named_struct_type U.context (T.Struct_name.to_string s)

(* ############################ Codegen expr ################################ *)
let rec codegen_expr (expr: D.dexpr) (sym_table: St.llvm_symbol_table) : (St.llvm_symbol_table * llvalue) =
  match expr.node with
  | D.DIntLit i -> (sym_table, const_int (Llvm.i32_type U.context) i)
  | D.DBoolLit b -> (sym_table, const_int (i1_type U.context) (if b then 1 else 0))
  | D.DStringLit s -> (sym_table, const_string U.context s)
  | DBinOp (op, lhs, rhs) ->
    let (_, lhs_val) = codegen_expr {expr with node = lhs} sym_table in
    let (_, rhs_val) = codegen_expr {expr with node = rhs} sym_table in
    begin 
      match op with 
      | T.BinOpPlus -> (sym_table, L.build_add lhs_val rhs_val "addtmp" U.builder)
      | T.BinOpMinus -> (sym_table, L.build_sub lhs_val rhs_val "subtmp" U.builder)
      | T.BinOpMult -> (sym_table, L.build_mul lhs_val rhs_val "multmp" U.builder)
      | T.BinOpIntDiv -> (sym_table, L.build_sdiv lhs_val rhs_val "divtmp" U.builder)
      | T.BinOpRem -> (sym_table, L.build_srem lhs_val rhs_val "remtmp" U.builder)
      | T.BinOpEq -> (sym_table, L.build_icmp L.Icmp.Eq lhs_val rhs_val "eqtmp" U.builder)
      | T.BinOpNotEq -> (sym_table, L.build_icmp L.Icmp.Ne lhs_val rhs_val "neqtmp" U.builder)
      | T.BinOpLessThan -> (sym_table, L.build_icmp L.Icmp.Slt lhs_val rhs_val "lttmp" U.builder)
      | T.BinOpLessThanEq -> (sym_table, L.build_icmp L.Icmp.Sle lhs_val rhs_val "lteqtmp" U.builder)
      | T.BinOpGreaterThan -> (sym_table, L.build_icmp L.Icmp.Sgt lhs_val rhs_val "gttmp" U.builder)
      | T.BinOpGreaterThanEq -> (sym_table, L.build_icmp L.Icmp.Sge lhs_val rhs_val "gteqtmp" U.builder)
      | T.BinOpAnd -> (sym_table, L.build_and lhs_val rhs_val "andtmp" U.builder)
      | T.BinOpOr -> (sym_table, L.build_or lhs_val rhs_val "ortmp" U.builder)
      end
  | DUnOp (op, un_expr) ->
    let (_, un_expr_val) = codegen_expr {expr with node = un_expr} sym_table in
    begin
      match op with
      | T.UnOpNeg -> (sym_table, L.build_neg un_expr_val "negtmp" U.builder)
      | T.UnOpNot -> (sym_table, L.build_not un_expr_val "nottmp" U.builder)
    end

  | DVar var_name ->
    begin
        match St.lookup_variable sym_table var_name with
        | Some (St.LVarInfo {llvm_value = Some value; llvm_type = Some typ; _}) when L.classify_type typ = L.TypeKind.Pointer ->
            let loaded_value = L.build_load value var_name U.builder in
            sym_table, loaded_value
        | Some (St.LVarInfo {llvm_value = Some value; _}) -> 
            sym_table, value
        | _ -> failwith (Printf.sprintf "Variable %s not found or not declared in the symbol table." var_name)
    end

  | D.DAssign (varname, rhs_expr_node) ->
    let (updated_sym_table, rhs_value) = 
        codegen_expr (wrap expr.loc expr.typ rhs_expr_node) sym_table in
    begin 
      match St.lookup_variable updated_sym_table varname with
      | Some _ ->
        let llvm_type = expr.typ |> llvm_type_of_typ in
        let llvm_var = L.build_alloca llvm_type varname U.builder in
        ignore (L.build_store rhs_value llvm_var U.builder);
        let updated_info = St.LVarInfo { 
            llvm_value = Some llvm_var; 
            llvm_type = Some llvm_type;
            storage = Local;
            is_global = false;
        } in
        let updated_sym_table = St.add_symbol updated_sym_table varname updated_info in
        (updated_sym_table, rhs_value)

      | _ -> raise (Codegen_error (Printf.sprintf "Variable %s not found" varname))
    end
  
  | DCall (fname, args) -> 
    begin
      match L.lookup_function fname U.the_module with
      | Some fn ->
          let arg_values_and_tables = List.map (fun arg -> codegen_expr {expr with node = arg} sym_table) args in
          let arg_values = List.map (fun (_, value) -> 
            if L.type_of value = L.pointer_type (L.i32_type U.context) then
              L.build_load value "load_tmp" U.builder
            else
              value
          ) arg_values_and_tables in
          let call = L.build_call fn (Array.of_list arg_values) fname U.builder in
          sym_table, call
      | None -> 
          failwith (Printf.sprintf "Function %s not declared in the LLVM module." fname)
    end
  
  
  

  (* | DBlockExpr exprs ->
    (* Codegen each expression in the block, preserving the sym_table *)
    let sym_table, _ = List.fold_left (fun (acc_sym_table, _) e ->
        codegen_expr {expr with node = e} acc_sym_table
    ) (sym_table, (L.const_null (L.void_type U.context))) exprs in
    sym_table, (L.const_null (L.void_type U.context))
   *)

  (* | DCreateThread (fname, args) ->
    begin
        match L.lookup_function fname U.the_module with
        | Some fn ->
            (* Generate arguments *)
            let arg_values_and_tables = List.map (fun arg -> 
                codegen_expr {expr with node = arg} sym_table
            ) args in

            let arg_values = List.map snd arg_values_and_tables in

            (* For simplicity, let's consider the function takes only one argument. 
                Adjust this based on your function's actual signature *)
            let arg_for_thread_func = List.hd arg_values in

            (* Get function reference for create_thread *)
            let create_thread_fn = L.lookup_function "create_thread" U.the_module in
            begin
            match create_thread_fn with
            | Some create_thread_fn_ref ->
                (* fn is the function pointer, and arg_for_thread_func is the argument *)
                let thread_id = L.build_call create_thread_fn_ref [| fn; arg_for_thread_func |] "thread_id" U.builder in
                sym_table, thread_id
            | None -> 
                failwith "create_thread function not found in the LLVM module."
            end

        | None -> 
            failwith (Printf.sprintf "Function %s not declared in the LLVM module." fname)
    end

  | DJoinThread e ->
    let _, thread_id_value = codegen_expr {expr with node = e} sym_table in

    let join_thread_fn = L.lookup_function "join_thread" U.the_module in
    begin
    match join_thread_fn with
    | Some join_thread_fn_ref ->
        let _ = L.build_call join_thread_fn_ref [| thread_id_value |] "" U.builder in
        sym_table, (L.const_null (L.void_type U.context)) 
    | None -> 
        failwith "join_thread function not found in the LLVM module."
    end *)
  
    
  

      
      
    
    

  | _ -> raise (Codegen_error "not implemented")

(* ############################ Codegen block ################################ *)
let codegen_block
(sym_table: St.llvm_symbol_table) (block: D.dblock): St.llvm_symbol_table = 
  List.fold_left (fun acc_sym_table expr ->
  let (updated_sym_table, _) = codegen_expr expr acc_sym_table  in
  updated_sym_table
  ) sym_table block

(* ########################### Codegen function ############################## *)
let codegen_proto (func: D.dfunction) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table * llvalue =
  let param_types = List.map (fun (typ, _) -> llvm_type_of_typ typ) func.params in
  let ret_type = llvm_type_of_typ func.ret_type in
  let ftype = L.function_type ret_type (Array.of_list param_types) in

  match L.lookup_function func.name U.the_module with
  | Some fn -> (sym_table, fn)
  | None -> 
    let new_fn = L.declare_function func.name ftype U.the_module in
    let new_info = St.LFuncInfo { llvm_function = Some new_fn; params = [] } in  (* Initialize params as empty for now *)
    let new_sym_table = { St.table = St.SymbolMap.add func.name new_info sym_table.table; parent = Some sym_table } in
    (new_sym_table, new_fn)

let codegen_func_body (func: D.dfunction) (fn: llvalue) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table =
  let bb = L.append_block U.context "entry" fn in
  L.position_at_end bb U.builder;

  let param_name_to_index = List.mapi (fun i (_ptype, pname) -> (pname, i)) func.params in

  let updated_sym_table = 
    List.fold_left (fun acc_sym_table (_ptype, name) ->
      let param_index = List.assoc name param_name_to_index in 
      let param_val = L.param fn param_index in
      let new_var_info = St.LVarInfo { llvm_value = Some param_val; llvm_type = None; storage = Local; is_global = false } in
      { St.table = St.SymbolMap.add name new_var_info acc_sym_table.St.table; parent = acc_sym_table.St.parent }
    ) sym_table func.params in
    
  let last_expr_result = 
    match List.rev func.body with
    | last_expr :: _ -> 
        let (_updated_sym_table, value) = codegen_expr last_expr updated_sym_table in
        Some value
    | [] -> None
  in
  
  begin match last_expr_result with
    | Some value -> ignore (L.build_ret value U.builder)
    | None -> ignore (L.build_ret_void U.builder) (* Only valid if function is supposed to return void *)
  end;
    
  Llvm_analysis.assert_valid_function fn;
  updated_sym_table

(* ############################ Codegen struct ############################## *)
(* let codegen_structs (structs: D.dstruct) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table =
  List.fold_left (fun current_table dstruct ->
    let struct_name = dstruct.name in
    
    (* Convert the list of field types to an array of LLVM types *)
    let llvm_field_types = Array.of_list (List.map (fun (_, field_type) -> llvm_type_of_typ field_type) dstruct.fields) in

    (* Define an LLVM structure type *)
    let llvm_struct_type = L.named_struct_type U.context struct_name in
    L.struct_set_body llvm_struct_type llvm_field_types false;
    
    (* Add the defined LLVM structure type to the symbol table *)
    let struct_info_option = St.lookup_variable current_table struct_name in
    begin match struct_info_option with
    | Some (St.LStructInfo struct_info) ->
        struct_info.St.llvm_struct <- Some llvm_struct_type;
        current_table
    | _ -> 
        failwith (Printf.sprintf "Struct %s not found in the symbol table or mismatched identifier type." struct_name)
    end
  ) sym_table structs *)


(* ############################# Codegen main ############################### *)
let codegen_main (main_block: D.dblock) (sym_table: St.llvm_symbol_table): unit =
  let main_function = match L.lookup_function "main" U.the_module with
    | Some fn -> fn
    | None -> 
      let ftype = L.function_type (L.void_type U.context) [||] in
      L.declare_function "main" ftype U.the_module in
  let bb = L.append_block U.context "entry" main_function in
  L.position_at_end bb U.builder;

  let _ = codegen_block sym_table main_block in
  
  if L.block_terminator (L.insertion_block U.builder) = None then
    ignore (L.build_ret_void U.builder);

  Llvm_analysis.assert_valid_function main_function

(* ########################### Codegen program ############################## *)

let codegen_ast (dprogram: D.dprogram) (symboltable: St.llvm_symbol_table): llmodule =   
  E.declare_externals U.the_module;
  (* Generate code for all structs *)
  (* let sym_table_0 = codegen_structs dprogram.structs symboltable in
 *)

  (* Generate code for all functions *)
  let sym_table_1 = List.fold_left (fun acc_sym_table func ->
    let (new_sym_table, fn) = codegen_proto func acc_sym_table in
    codegen_func_body func fn new_sym_table
  ) symboltable dprogram.functions in

  (* Generate code for the main block *)
  let _ = codegen_main dprogram.main sym_table_1 in
  U.the_module