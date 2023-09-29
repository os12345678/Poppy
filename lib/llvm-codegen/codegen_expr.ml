open Llvm
(* open Codegen_util *)
(* open Desugar.Desugared_ast *)

exception Codegen_error of string

module L = Llvm
module D = Desugar.Desugared_ast
module U = Codegen_util
module T = Poppy_parser.Ast_types
module St = Ir_symbol_table
module M = Codegen_util
module E = Link_extern

let wrap loc typ node = {
  D.loc = loc;
  D.typ = typ;
  D.node = node;
}


let llvm_type_of_typ = function
  | T.TEInt -> Llvm.i32_type M.context
  | T.TEBool -> Llvm.i1_type M.context
  | T.TEVoid -> Llvm.void_type M.context
  | T.TEStruct s -> Llvm.named_struct_type M.context (T.Struct_name.to_string s)

(* ############################ Codegen expr ################################ *)
let rec codegen_expr (expr: D.dexpr) (sym_table: St.llvm_symbol_table) : (St.llvm_symbol_table * llvalue) =
  match expr.node with
  | D.DIntLit i -> (sym_table, const_int (Llvm.i32_type M.context) i)
  | D.DBoolLit b -> (sym_table, const_int (i1_type M.context) (if b then 1 else 0))
  | D.DStringLit s -> (sym_table, const_string M.context s)
  | DBinOp (op, lhs, rhs) ->
    let (_, lhs_val) = codegen_expr {expr with node = lhs} sym_table in
    let (_, rhs_val) = codegen_expr {expr with node = rhs} sym_table in
    begin 
      match op with 
      | T.BinOpPlus -> (sym_table, L.build_add lhs_val rhs_val "addtmp" M.builder)
      | T.BinOpMinus -> (sym_table, L.build_sub lhs_val rhs_val "subtmp" M.builder)
      | T.BinOpMult -> (sym_table, L.build_mul lhs_val rhs_val "multmp" M.builder)
      | T.BinOpIntDiv -> (sym_table, L.build_sdiv lhs_val rhs_val "divtmp" M.builder)
      | T.BinOpRem -> (sym_table, L.build_srem lhs_val rhs_val "remtmp" M.builder)
      | T.BinOpEq -> (sym_table, L.build_icmp L.Icmp.Eq lhs_val rhs_val "eqtmp" M.builder)
      | T.BinOpNotEq -> (sym_table, L.build_icmp L.Icmp.Ne lhs_val rhs_val "neqtmp" M.builder)
      | T.BinOpLessThan -> (sym_table, L.build_icmp L.Icmp.Slt lhs_val rhs_val "lttmp" M.builder)
      | T.BinOpLessThanEq -> (sym_table, L.build_icmp L.Icmp.Sle lhs_val rhs_val "lteqtmp" M.builder)
      | T.BinOpGreaterThan -> (sym_table, L.build_icmp L.Icmp.Sgt lhs_val rhs_val "gttmp" M.builder)
      | T.BinOpGreaterThanEq -> (sym_table, L.build_icmp L.Icmp.Sge lhs_val rhs_val "gteqtmp" M.builder)
      | T.BinOpAnd -> (sym_table, L.build_and lhs_val rhs_val "andtmp" M.builder)
      | T.BinOpOr -> (sym_table, L.build_or lhs_val rhs_val "ortmp" M.builder)
      end
  | DUnOp (op, un_expr) ->
    let (_, un_expr_val) = codegen_expr {expr with node = un_expr} sym_table in
    begin
      match op with
      | T.UnOpNeg -> (sym_table, L.build_neg un_expr_val "negtmp" M.builder)
      | T.UnOpNot -> (sym_table, L.build_not un_expr_val "nottmp" M.builder)
    end

    | DVar var_name ->
      begin
          match St.lookup_variable sym_table var_name with
          | Some (St.LVarInfo {llvm_value = Some value; llvm_type = Some typ; _}) when L.classify_type typ = L.TypeKind.Pointer ->
              let loaded_value = L.build_load value var_name M.builder in
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
        | Some (St.LVarInfo { llvm_value = Some llvm_var; _ }) ->
            ignore (L.build_store rhs_value llvm_var M.builder); 
            (updated_sym_table, rhs_value)

        | Some (LVarInfo { llvm_value = None; llvm_type = None; storage = Local; _ }) ->
          let llvm_type = expr.typ |> llvm_type_of_typ in
          let llvm_var = Llvm.build_alloca llvm_type varname M.builder in
          ignore (L.build_store rhs_value llvm_var M.builder);
          let updated_info = St.LVarInfo { 
              llvm_value = Some llvm_var; 
              llvm_type = Some llvm_type;
              storage = Local;
              is_global = false;
          } in
          let updated_sym_table = St.add_symbol sym_table varname updated_info in
          (updated_sym_table, rhs_value)

        | _ -> raise (Codegen_error (Printf.sprintf "Variable %s not found" varname))
      end

      | DCall (fname, args) -> 
        begin
          match L.lookup_function fname M.the_module with
          | Some fn ->
              let arg_values_and_tables = List.map (fun arg -> codegen_expr {expr with node = arg} sym_table) args in
              let arg_values = List.map (fun (_, value) -> 
                match L.classify_value value with
                | L.ValueKind.GlobalVariable -> 
                    value
                | _ -> 
                    if L.type_of value = L.pointer_type (L.i8_type M.context) then 
                        let global_string = L.define_global "str_const" value M.the_module in
                        L.build_gep global_string [|L.const_int (L.i32_type M.context) 0|] "str_ptr" M.builder
                    else if L.type_of value = L.pointer_type (L.i32_type M.context) then
                        L.build_load value "load_tmp" M.builder
                    else
                        value
              ) arg_values_and_tables in
              let call = L.build_call fn (Array.of_list arg_values) fname M.builder in
              sym_table, call
          | None -> 
              failwith (Printf.sprintf "Function %s not declared in the LLVM module." fname)
        end
    
    

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

  match L.lookup_function func.name M.the_module with
  | Some fn -> (sym_table, fn)
  | None -> 
    let new_fn = L.declare_function func.name ftype M.the_module in
    let new_info = St.LFuncInfo { llvm_function = Some new_fn; params = [] } in  (* Initialize params as empty for now *)
    let new_sym_table = { St.table = St.SymbolMap.add func.name new_info sym_table.table; parent = Some sym_table } in
    (new_sym_table, new_fn)

let codegen_func_body (func: D.dfunction) (fn: llvalue) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table =
  let bb = L.append_block M.context "entry" fn in
  L.position_at_end bb M.builder;

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
    | Some value -> ignore (L.build_ret value M.builder)
    | None -> ignore (L.build_ret_void M.builder) (* Only valid if function is supposed to return void *)
  end;
    
  Llvm_analysis.assert_valid_function fn;
  updated_sym_table

(* ############################ Codegen struct ############################## *)


(* ############################# Codegen main ############################### *)
let codegen_main (main_block: D.dblock) (sym_table: St.llvm_symbol_table): unit =
  let main_function = match L.lookup_function "main" M.the_module with
    | Some fn -> fn
    | None -> 
      let ftype = L.function_type (L.void_type M.context) [||] in
      L.declare_function "main" ftype M.the_module in
  let bb = L.append_block M.context "entry" main_function in
  L.position_at_end bb M.builder;

  let _ = codegen_block sym_table main_block in
  
  if L.block_terminator (L.insertion_block M.builder) = None then
    ignore (L.build_ret_void M.builder);

  Llvm_analysis.assert_valid_function main_function

(* ########################### Codegen program ############################## *)

let codegen_ast (dprogram: D.dprogram) (symboltable: St.llvm_symbol_table): llmodule =   
  E.declare_externals M.the_module;
  (* Generate code for all structs *)

  (* Generate code for all functions *)
  let _ = List.fold_left (fun acc_sym_table func ->
    let (new_sym_table, fn) = codegen_proto func acc_sym_table in
    codegen_func_body func fn new_sym_table
  ) symboltable dprogram.functions in

  (* Generate code for the main block *)
  let _ = codegen_main dprogram.main symboltable in
  M.the_module