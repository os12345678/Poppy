(* open Llvm
open Codegen_util
open Desugar.Desugared_ast

exception Codegen_error of string

module D = Desugar.Desugared_ast
module A = Poppy_parser.Ast_types

(* ############################ Init llvm vtable ############################ *)
type symbol_info = {
  llvm_type : Llvm.lltype;
  llvm_value : Llvm.llvalue option;  (* For local variables *)
}

module Symbol_table = Map.Make(String)
type llvm_symbol_table = symbol_info Symbol_table.t

let string_of_symbol_type typ = 
  Printf.sprintf "Type: %s"
  (Llvm.string_of_lltype typ)

let string_of_llvm_type lltype = 
  (* Convert the Llvm.lltype to a string representation *)
  (* For simplicity, using Llvm.string_of_lltype, you can use a more comprehensive method if needed *)
  Llvm.string_of_lltype lltype

let string_of_llvm_value_option llvalue_option = 
  match llvalue_option with
  | Some llvalue -> Llvm.string_of_llvalue llvalue
  | None -> "None"

let string_of_symbol_info { llvm_type; llvm_value } =
  Printf.sprintf "Type: %s, Value: %s" (string_of_llvm_type llvm_type) (string_of_llvm_value_option llvm_value)

let print_symbol_table (table: llvm_symbol_table) : unit =
  Printf.printf "Symbol Table:\n";
  Symbol_table.iter (fun key value ->
      Printf.printf "Identifier: %s, Info: %s\n" key (string_of_symbol_info value)
    ) table;
  Printf.printf "\n"
  
  
(* ############################ Codegen Program ############################# *)
module Codegen : sig
  val codegen_ast : dprogram -> llvm_symbol_table
end = struct

let symbol_table : llvm_symbol_table ref = ref Symbol_table.empty

let lookup_symbol name =
  Symbol_table.find_opt name !symbol_table
    
let rec codegen_ast (program: dprogram) : llvm_symbol_table =
  List.iter codegen_struct program.structs;
  List.iter codegen_function_decl program.functions;
  ignore (codegen_block program.main);
  !symbol_table 

(* ############################# Codegen Expr ############################### *)

and codegen_expr (expr: D.dexpr) : llvm_symbol_table = 
  match expr.node with 
    | DIntLit _ -> !symbol_table
    | DBoolLit _ -> !symbol_table
    | DStringLit _ -> !symbol_table
    | DBinOp (_, _, _) -> !symbol_table
    | DUnOp (_, _) -> !symbol_table
    | DBlockExpr exprs -> 
      List.iter (fun e -> ignore (codegen_expr { expr with node = e })) exprs;
      !symbol_table
    | DVar name ->
      (match lookup_symbol name with
      | Some _ -> !symbol_table
      | None -> failwith (Printf.sprintf "Variable %s not declared." name))
    | DAssign (name, node) ->
      let _ = codegen_expr { expr with node = node } in
      let var_type = type_to_llvm expr.typ symbol_table in
      let new_symbol_info = { llvm_type = var_type; llvm_value = None } in
      symbol_table := Symbol_table.add name new_symbol_info !symbol_table;
      !symbol_table
    | DCall (function_name, arg_exprs) ->
      if lookup_symbol function_name = None then
        failwith ("Undefined function: " ^ function_name);
      List.iter (fun arg_expr -> ignore(codegen_expr {expr with node = arg_expr})) arg_exprs;
      !symbol_table
    | DIf (condition_expr, true_block, false_block) ->
      ignore(codegen_expr {expr with node = condition_expr});
      ignore(codegen_block true_block);
      ignore(codegen_block false_block);
      !symbol_table
    | DWhile (condition_expr, loop_block) ->
      ignore(codegen_expr {expr with node = condition_expr});
      ignore(codegen_block loop_block);
      !symbol_table
    | DCreateThread (function_name, arg_exprs) ->
      (match lookup_symbol function_name with
      | None -> failwith ("Undefined function for thread creation: " ^ function_name)
      | Some _function_info ->
          (* Process each argument expression *)
          List.iter (fun arg_expr -> ignore(codegen_expr {expr with node = arg_expr})) arg_exprs);
     !symbol_table  (* Return updated symbol table *)
    
    | DJoinThread thread_expr ->
      (* Process the thread identifier expression *)
      ignore(codegen_expr {expr with node = thread_expr});

      !symbol_table  (* Return updated symbol table *)

(* ############################# Codegen Block ############################## *)
and codegen_block (block: dblock) : llvm_symbol_table =
  List.iter (fun expr -> 
    ignore (codegen_expr expr)
  ) block;
  !symbol_table

(* ############################# Codegen Struct ############################# *)
and codegen_struct (s: dstruct) : unit =
  let llvm_field_types = List.map (fun (_, t) -> type_to_llvm t symbol_table) s.fields in
  let llvm_struct_type = Llvm.struct_type context (Array.of_list llvm_field_types) in
  let symbol_info = { llvm_type = llvm_struct_type; llvm_value = None } in
  symbol_table := Symbol_table.add ("init_"^s.name) symbol_info !symbol_table;

(* ############################ Codegen Function ############################ *)
and codegen_function_decl (f: dfunction) : unit =
  let llvm_arg_types = List.map (fun (t, _) -> type_to_llvm t symbol_table) f.params in
  let llvm_ret_type = type_to_llvm f.ret_type symbol_table in
  let llvm_func_type = Llvm.function_type llvm_ret_type (Array.of_list llvm_arg_types) in
  let llvm_func = Llvm.declare_function f.name llvm_func_type the_module in
  let symbol_info = { llvm_type = llvm_func_type; llvm_value = Some llvm_func } in
  symbol_table := Symbol_table.add f.name symbol_info !symbol_table;
 
(* ########################### Type_expr -> lltype ########################### *)
and type_to_llvm (t: T.type_expr) (symbol_table: llvm_symbol_table ref) : Llvm.lltype = 
  match t with
  | TEInt -> Llvm.i32_type context
  | TEBool -> Llvm.i1_type context
  | TEStruct sname -> 
      let name = A.Struct_name.to_string sname in
      (match Symbol_table.find_opt name !symbol_table with
      | Some symbol_info -> symbol_info.llvm_type
      | None -> failwith ("Unknown struct type: " ^ name))
  | TEVoid -> Llvm.void_type context
  end *)