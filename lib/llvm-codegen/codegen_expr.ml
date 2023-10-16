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

(* ############################ Codegen expr ################################ *)
let rec codegen_expr (expr: D.dexpr) (sym_table: St.llvm_symbol_table) (fpm: [ `Function ] L.PassManager.t) : (St.llvm_symbol_table * llvalue) =
  match expr.node with
  | D.DIntLit i -> (sym_table, const_int (Llvm.i32_type U.context) i)
  | D.DBoolLit b -> (sym_table, const_int (i1_type U.context) (if b then 1 else 0))
  | D.DStringLit s -> (sym_table, const_string U.context s)
  | DBinOp (op, lhs, rhs) ->
    let (_, lhs_val) = codegen_expr {expr with node = lhs} sym_table fpm in
    let (_, rhs_val) = codegen_expr {expr with node = rhs} sym_table fpm in
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
    let (_, un_expr_val) = codegen_expr {expr with node = un_expr} sym_table fpm in
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

  | DAssign (varname, rhs_expr_node) ->
    let (updated_sym_table, rhs_value) = 
        codegen_expr (U.wrap expr.loc expr.typ rhs_expr_node) sym_table fpm in
    begin 
      match St.lookup_variable updated_sym_table varname with
      | Some _ ->
        let llvm_type = expr.typ |> U.llvm_type_of_typ in
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

    (* | DAssign (name, e) ->
    let wrapped_expr = { loc = expr.loc; typ = expr.typ; node = e } in
    let updated_sym_table, rhs_value = process_expr sym_table wrapped_expr in
    let var_info = lookup_variable updated_sym_table name in 
    begin 
        match var_info with 
        | Some (LVarInfo {llvm_value = Some llvm_var; _}) ->
            ignore (L.build_store rhs_value llvm_var U.builder);
            updated_sym_table
        | None ->
            (* If the variable does not exist in the symbol table, you can decide how to handle it.
               Either raise an error, create a new variable or some other handling. *)
            let llvm_type = wrapped_expr.typ |> U.llvm_type_of_typ in
            let llvm_var = L.build_alloca llvm_type name U.builder in
            ignore (L.build_store rhs_value llvm_var U.builder);
            let new_var_info = LVarInfo { llvm_value = Some llvm_var; llvm_type = Some llvm_type; storage = Local; is_global = false } in
            add_symbol updated_sym_table name new_var_info
        | Some (LFuncInfo _ | LStructInfo _) ->
            failwith (Printf.sprintf "Identifier %s is not a variable, but type-checking phase passed!!!" name)
    end
 *)
  
  | DCall (fname, args) -> 
    begin
      match L.lookup_function fname U.the_module with
      | Some fn ->
          let arg_values_and_tables = List.map (fun arg -> codegen_expr {expr with node = arg} sym_table fpm) args in
          let arg_values = List.map (fun (_, value) -> 
            match L.classify_value value with
            | L.ValueKind.ConstantDataArray ->
                let string_ptr = U.handle_string_constant value in
                string_ptr
            | _ -> 
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
  
  | DIf (cond, then_block, else_block) ->
    let (sym_table_after_cond, cond_val) = codegen_expr { expr with node = cond } sym_table fpm in
    
    let start_bb = L.insertion_block U.builder in
    let the_function = L.block_parent start_bb in
    let then_bb = L.append_block U.context "then" the_function in
    let else_bb = L.append_block U.context "else" the_function in
    let merge_bb = L.append_block U.context "ifcont" the_function in

    ignore (L.build_cond_br cond_val then_bb else_bb U.builder);

    L.position_at_end then_bb U.builder;
    let _ = codegen_block then_block sym_table_after_cond fpm in
    if L.block_terminator (L.insertion_block U.builder) = None then
      ignore (L.build_br merge_bb U.builder);

    L.position_at_end else_bb U.builder;
    let _ = codegen_block else_block sym_table_after_cond fpm in
    if L.block_terminator (L.insertion_block U.builder) = None then
      ignore (L.build_br merge_bb U.builder);

    L.position_at_end merge_bb U.builder;
    sym_table_after_cond, L.const_int (L.i32_type U.context) 0

  | DWhile (cond, block) ->
    let start_bb = L.insertion_block U.builder in
    let the_function = L.block_parent start_bb in
    let loop_header = L.append_block U.context "loop.header" the_function in
    let loop_body = L.append_block U.context "loop.body" the_function in
    let loop_exit = L.append_block U.context "loop.exit" the_function in

    ignore (L.build_br loop_header U.builder);

    L.position_at_end loop_header U.builder;
    let (sym_table_after_cond, cond_val) = codegen_expr { expr with node = cond } sym_table fpm in

    ignore (L.build_cond_br cond_val loop_body loop_exit U.builder);

    L.position_at_end loop_body U.builder;
    let _ = codegen_block block sym_table_after_cond fpm in
    if L.block_terminator (L.insertion_block U.builder) = None then
      ignore (L.build_br loop_header U.builder);  (* Jump back to loop header. *)

    L.position_at_end loop_exit U.builder;

    sym_table_after_cond, L.const_int (L.i32_type U.context) 0

  | DBlockExpr exprs ->
    let loc = expr.loc in
    let typ = expr.typ in
    let wrapped = List.map (fun expr_node -> U.wrap loc typ expr_node) exprs in
    codegen_block wrapped sym_table fpm

    | DCreateThread (fname, args) ->
      begin
          match L.lookup_function fname U.the_module with
          | Some _ ->
              let arg_values_and_tables = List.map (fun arg -> 
                  codegen_expr {expr with node = arg} sym_table fpm
              ) args in
  
              let arg_values = List.map snd arg_values_and_tables in
              let arg_types = List.map (fun arg -> L.type_of (snd arg)) arg_values_and_tables in
  
              let packed_args = U.pack_args arg_values arg_types in
  
              let wrapper = U.generate_wrapper_function fname arg_types (U.llvm_type_of_typ expr.typ) in
  
              let create_thread_fn = L.lookup_function "create_thread" U.the_module in
              begin
              match create_thread_fn with
              | Some create_thread_fn_ref ->
                  let thread_id = L.build_call create_thread_fn_ref [| wrapper; packed_args |] "thread_id" U.builder in
                  U.add_thread_to_table fname thread_id;
                  sym_table, thread_id
              | None -> 
                  failwith "create_thread function not found in the LLVM module."
              end
          | None -> 
              failwith (Printf.sprintf "Function %s not declared in the LLVM module." fname)
      end
  
  
  | DJoinThread thread_name ->
    begin
      match thread_name with 
      | DVar(name) -> 
        begin
          match U.check_and_remove_thread_from_table name with
          | Some thread_id_value ->
              (* Get function reference for join_thread *)
              let join_thread_fn = L.lookup_function "join_thread" U.the_module in
              begin
                match join_thread_fn with
                | Some join_thread_fn_ref ->
                    let t = L.build_call join_thread_fn_ref [| thread_id_value |] "" U.builder in
                    (* Return a null void value as the resulting expression *)
                    sym_table, t;
                | None -> 
                    failwith "join_thread function not found in the LLVM module."
              end
          | None -> 
              failwith (Printf.sprintf "No active threads found for function: %s" name)
          end
      | _ ->
          failwith "Invalid thread name"
    end
        
(* ############################ Codegen block ################################ *)
and codegen_block (block: D.dblock) (sym_table: St.llvm_symbol_table) (fpm: [ `Function ] L.PassManager.t) : St.llvm_symbol_table * llvalue =
  match block with
  | [] -> (sym_table, L.const_int (L.i32_type U.context) 0) (* Default return for an empty block. Adjust as needed. *)
  | [last_expr] -> codegen_expr last_expr sym_table fpm
  | expr :: rest -> 
      let updated_sym_table, _ = codegen_expr expr sym_table fpm in
      codegen_block rest updated_sym_table fpm

(* ########################### Codegen function ############################## *)
let codegen_proto (func: D.dfunction) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table * llvalue =
  let param_types = List.map (fun (typ, _) -> U.llvm_type_of_typ typ) func.params in
  let ret_type = U.llvm_type_of_typ func.ret_type in
  let ftype = L.function_type ret_type (Array.of_list param_types) in

  match L.lookup_function func.name U.the_module with
  | Some fn -> (sym_table, fn)
  | None -> 
    let new_fn = L.declare_function func.name ftype U.the_module in
    let new_info = St.LFuncInfo { llvm_function = Some new_fn; params = [] } in  (* Initialize params as empty for now *)
    let new_sym_table = { St.table = St.SymbolMap.add func.name new_info sym_table.table; parent = Some sym_table } in
    (new_sym_table, new_fn)

let codegen_func_body (func: D.dfunction) (fn: llvalue) (sym_table: St.llvm_symbol_table) (fpm: [ `Function ] L.PassManager.t) : St.llvm_symbol_table =
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
        let (_updated_sym_table, value) = codegen_expr last_expr updated_sym_table fpm in
        Some value
    | [] -> None
  in
  
  begin match last_expr_result with
    | Some value -> ignore (L.build_ret value U.builder)
    | None -> ignore (L.build_ret_void U.builder) (* Only valid if function is supposed to return void *)
  end;
    
  Llvm_analysis.assert_valid_function fn;

  (* let _ = PassManager.run_function fn U.the_fpm in *)
  updated_sym_table

(* ############################ Codegen struct ############################## *)
let codegen_struct_init_function dstruct llvm_struct_type =
  let func_name = "init_" ^ dstruct.D.name in
  let param_types = Array.of_list (List.map U.llvm_type_of_typ (List.map snd dstruct.fields)) in
  let ret_type = L.pointer_type llvm_struct_type in
  let func_type = L.function_type ret_type param_types in
  let func = L.define_function func_name func_type U.the_module in
  
  let builder = L.builder_at_end U.context (L.entry_block func) in
  
  (* Allocate memory for the struct on the stack *)
  let alloca = L.build_malloc llvm_struct_type "" builder in

  (* Populate the struct's fields *)
  List.iteri (fun idx (_, _field_typ) ->
      let field_ptr = L.build_struct_gep alloca idx "" builder in
      let param_value = L.param func idx in
      let _ = L.build_store param_value field_ptr builder in
      ()
  ) dstruct.fields;

  (* Return the pointer to the struct *)
  let _ = L.build_ret alloca builder in
  func

let codegen_structs (structs: D.dstruct list) (sym_table: St.llvm_symbol_table) : St.llvm_symbol_table  =
  List.fold_left (fun current_table dstruct ->
    let struct_name = dstruct.D.name in
    let field_types = List.map snd dstruct.fields in
    let llvm_field_types = List.map (fun t -> U.llvm_type_of_typ t) field_types in
    let llvm_struct_type = L.struct_type U.context (Array.of_list llvm_field_types) in
    let _ = codegen_struct_init_function dstruct llvm_struct_type in
    let struct_info = St.LStructInfo {
      llvm_struct = Some llvm_struct_type; 
      field_map = (match St.lookup_variable current_table struct_name with
                   | Some (St.LStructInfo info) -> info.field_map
                   | _ -> failwith "Struct information not found in symbol table.")
    } in
    St.add_symbol current_table struct_name struct_info
  ) sym_table structs



(* ############################# Codegen main ############################### *)
let codegen_main (main_block: D.dblock) (sym_table: St.llvm_symbol_table) (fpm: [ `Function ] L.PassManager.t) : unit =
  let main_function = match L.lookup_function "main" U.the_module with
    | Some fn -> fn
    | None -> 
      let ftype = L.function_type (L.void_type U.context) [||] in
      L.declare_function "main" ftype U.the_module in
  let bb = L.append_block U.context "entry" main_function in
  L.position_at_end bb U.builder;

  let _ = codegen_block main_block sym_table fpm in
  
  if L.block_terminator (L.insertion_block U.builder) = None then
    ignore (L.build_ret_void U.builder);

  Llvm_analysis.assert_valid_function main_function

(* ########################### Codegen program ############################## *)

let codegen_ast (dprogram: D.dprogram) (symboltable: St.llvm_symbol_table) (fpm: [ `Function ] L.PassManager.t): llmodule =   
  E.declare_externals U.the_module;

  (* Generate code for all structs *)
  let updated_sym_table = codegen_structs dprogram.structs symboltable in

  (* Generate code for all functions *)
  let sym_table_1 = List.fold_left (fun acc_sym_table func ->
    let (new_sym_table, fn) = codegen_proto func acc_sym_table in
    codegen_func_body func fn new_sym_table fpm
  ) updated_sym_table dprogram.functions in

  (* Generate code for the main block *)
  let _ = codegen_main dprogram.main sym_table_1 fpm in

  Llvm_analysis.assert_valid_module U.the_module;
  U.the_module