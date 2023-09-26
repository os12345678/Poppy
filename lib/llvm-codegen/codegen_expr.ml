open Llvm
(* open Codegen_util *)
(* open Desugar.Desugared_ast *)

exception Codegen_error of string

module L = Llvm
module D = Desugar.Desugared_ast
module U = Codegen_util
module T = Poppy_parser.Ast_types
module St = Ir_symbol_table

let wrap loc typ node = {
  D.loc = loc;
  D.typ = typ;
  D.node = node;
}
let context = Llvm.global_context ()
let the_module = Llvm.create_module context "Poppy JIT"
let builder = Llvm.builder context

let llvm_type_of_typ = function
  | T.TEInt -> Llvm.i32_type context
  | T.TEBool -> Llvm.i1_type context
  | T.TEVoid -> Llvm.void_type context
  | T.TEStruct s -> Llvm.named_struct_type context (T.Struct_name.to_string s)

(* ############################ Codegen expr ################################ *)
(* module St = struct *)

let rec codegen_expr
(sym_table: St.llvm_symbol_table) (expr: D.dexpr): (St.llvm_symbol_table * llvalue) =
  match expr.node with
  | D.DIntLit i -> (sym_table, const_int (Llvm.i32_type context) i)
  | D.DBoolLit b -> (sym_table, const_int (i1_type context) (if b then 1 else 0))
  | D.DStringLit s -> (sym_table, const_string context s)

  (* | D.DVar varname -> 
    (match St.lookup_variable (ref sym_table) varname with
    | Some (St.LVarInfo { llvm_value = Some llvm_var; _ }) -> Llvm.build_load llvm_var varname builder
    | Some (St.LVarInfo { llvm_value = None; _ }) -> 
      let llvm_type = expr.typ |> llvm_type_of_typ in
      let llvm_var = Llvm.build_alloca llvm_type varname builder in
      let _ = St.add_symbol (ref sym_table) varname (St.LVarInfo 
          { llvm_value = Some llvm_var; llvm_type = Some llvm_type; storage=Local; is_global=false}) in
      raise (Codegen_error (Printf.sprintf "Variable %s not a loadable value" varname))
    | _ -> raise (Codegen_error (Printf.sprintf "Variable %s not found" varname))) *)

    | D.DAssign (varname, rhs_expr_node) ->
      (* Generate code for the right-hand side of the assignment *)
      let (updated_sym_table, rhs_value) = 
          codegen_expr sym_table (wrap expr.loc expr.typ rhs_expr_node) in
      (* Lookup the variable in the symbol table to get its location *)
      begin 
        match St.lookup_variable updated_sym_table varname with
        | Some (St.LVarInfo { llvm_value = Some llvm_var; _ }) ->
            (* Known and Allocated case *)
            ignore (L.build_store rhs_value llvm_var builder); 
            (updated_sym_table, rhs_value)

        | Some (LVarInfo { llvm_value = None; llvm_type = None; storage = Local; _ }) ->
          (* Known Local variable but Unallocated *)
          let llvm_type = expr.typ |> llvm_type_of_typ in
          print_endline (Llvm.string_of_lltype llvm_type);
          print_endline "Allocating variable %s\n";
          Llvm.string_of_llmodule the_module |> print_endline;
          let llvm_var = Llvm.build_alloca llvm_type varname builder in
          print_endline "Allocated variable %s\n";
          ignore (L.build_store rhs_value llvm_var builder);
          print_endline "Stored value %s\n";
          let updated_info = St.LVarInfo { 
              llvm_value = Some llvm_var; 
              llvm_type = Some llvm_type;
              storage = Local;
              is_global = false;
          } in
          let updated_sym_table = St.add_symbol sym_table varname updated_info in
          (updated_sym_table, rhs_value)

        | _ -> 
            (* Unknown state *)
            raise (Codegen_error (Printf.sprintf "Variable %s not found" varname))
      end
  

  | _ -> raise (Codegen_error "not implemented")

let codegen_block
(sym_table: St.llvm_symbol_table) (block: D.dblock): St.llvm_symbol_table = 
  List.fold_left (fun acc_sym_table expr ->
  let (updated_sym_table, _) = codegen_expr acc_sym_table expr in
  updated_sym_table
  ) sym_table block

let codegen_ast (dprogram: D.dprogram) (symboltable: St.llvm_symbol_table): llmodule =   
  (* Generate code for all structs *)
  (* Generate code for all functions *)
  (* Generate code for the main block *)
  let _ = codegen_block symboltable dprogram.main in
  the_module