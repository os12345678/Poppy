open Llvm
open Codegen_util
open Desugar.Desugared_ast

exception Codegen_error of string

module D = Desugar.Desugared_ast
module U = Codegen_util
module E = Poppy_type_checker.Type_env
module A = Poppy_parser.Ast_types

(* ##################### Module and Context Creation ######################## *)
let context = context
let builder = builder
let the_module = the_module

let () = declare_thread_functions the_module

(* ############################ Init llvm vtable ############################ *)
type symbol_info = {
  llvm_type: Llvm.lltype;   (* The type of the identifier *)
  llvm_value: Llvm.llvalue; (* The value (or memory location) associated with the identifier *)
}
module Symbol_table = Map.Make(String)
type llvm_symbol_table = symbol_info Symbol_table.t

let string_of_symbol_info (info: symbol_info) : string =
  Printf.sprintf "Type: %s, Value: %s"
    (string_of_lltype info.llvm_type)
    (string_of_llvalue info.llvm_value)

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
let scope_stack : llvm_symbol_table list ref = ref []

(* let push_scope scope_stack = 
  Symbol_table.empty :: scope_stack

let push_new_scope () =
  scope_stack := Symbol_table.empty :: !scope_stack

let pop_scope () =
  match !scope_stack with
  | [] -> failwith "Scope stack should never be empty."
  | _current_scope :: rest ->
      scope_stack := rest *)
  
let add_to_current_scope name info =
  match !scope_stack with
  | [] -> failwith "Scope stack should never be empty."
  | current_scope :: rest ->
      let updated_scope = Symbol_table.add name info current_scope in
      scope_stack := updated_scope :: rest;
    updated_scope

let rec find_symbol name = function
| [] -> None
| scope :: rest ->
    match Symbol_table.find_opt name scope with
    | Some info -> Some info
    | None -> find_symbol name rest

let lookup_symbol name =
  find_symbol name !scope_stack
    
  let rec codegen_ast (program: dprogram) : llvm_symbol_table =
    (* Process structures *)
    List.iter codegen_struct program.structs;
  
    (* Process function declarations *)
    List.iter codegen_function_decl program.functions;
  
    (* Process the main block *)
    print_symbol_table !symbol_table;
    ignore (codegen_block program.main);
  
    !symbol_table  (* Return the updated symbol table *)
  

(* ############################# Codegen Expr ############################### *)

and codegen_expr (expr: D.dexpr) : llvalue * llvm_symbol_table = 
  match expr.node with 
    | DIntLit i -> 
      (Llvm.const_int (type_to_llvm A.TEInt symbol_table) i, !symbol_table)
    | DBoolLit b ->
      let llvm_const = Llvm.const_int (type_to_llvm A.TEBool symbol_table) (if b then 1 else 0) in
      (llvm_const, !symbol_table)
    | DStringLit s ->
      let llvm_const = Llvm.build_global_stringptr s "tmpstr" builder in
      (llvm_const, !symbol_table)
    | DBinOp (op, left_expr, right_expr) ->
      let (left_value, _) = codegen_expr {expr with node = left_expr} in
      let (right_value, _) = codegen_expr {expr with node = right_expr} in
      let bin_result = match op with
      | T.BinOpPlus -> build_add left_value right_value "addtmp" builder
      | T.BinOpMinus -> build_sub left_value right_value "subtmp" builder
      | T.BinOpMult -> build_mul left_value right_value "multmp" builder
      | T.BinOpIntDiv -> build_fdiv left_value right_value "divtmp" builder
      | T.BinOpRem -> build_frem left_value right_value "remtmp" builder
      | T.BinOpGreaterThan -> build_icmp Icmp.Sgt left_value right_value "sgttmp" builder 
      | T.BinOpGreaterThanEq -> build_icmp Icmp.Sge left_value right_value "sgetmp" builder
      | T.BinOpLessThan -> build_icmp Icmp.Slt left_value right_value "slttmp" builder
      | T.BinOpLessThanEq -> build_icmp Icmp.Sle left_value right_value "sletmp" builder
      | T.BinOpEq -> build_icmp Icmp.Eq left_value right_value "eqtmp" builder
      | T.BinOpNotEq -> build_icmp Icmp.Ne left_value right_value "netmp" builder
      | T.BinOpAnd -> build_and left_value right_value "andtmp" builder
      | T.BinOpOr -> build_or left_value right_value "ortmp" builder
      in
      (bin_result, !symbol_table)
    | DUnOp (op, operand_expr) ->
      let (operand_value, _) = codegen_expr {expr with node = operand_expr} in
      let un_result = match op with
      | T.UnOpNeg -> build_neg operand_value "negtmp" builder
      | T.UnOpNot -> build_not operand_value "nottmp" builder
      in
      (un_result, !symbol_table)
    | DVar name ->
      (match lookup_symbol name with
        | Some info -> (info.llvm_value, !symbol_table)
        | None -> failwith (Printf.sprintf "Variable %s not declared." name))
    | DAssign (name, node) ->
      let (value, _) = codegen_expr {expr with node = node} in
      (match lookup_symbol name with
      | Some info ->
          (* Variable exists, so just store the evaluated expression value in its location *)
          ignore (Llvm.build_store value info.llvm_value builder);
          (value, !symbol_table)
      | None ->
          (* Variable doesn't exist, so declare and allocate *)
          let var_type = type_to_llvm expr.typ symbol_table in
          let alloc = Llvm.build_alloca var_type name builder in
          let info = { llvm_type = var_type; llvm_value = alloc } in
          let updated_symbol_table = add_to_current_scope name info in
          (* Store the value *)
          ignore (Llvm.build_store value alloc builder);
          (* Return value and updated symbol table
          let updated_symbol_table = match !scope_stack with
          | current_scope :: _ -> current_scope
          | [] -> failwith "Scope stack should never be empty."
          in *)
          (value, updated_symbol_table))
    | DCall (function_name, arg_exprs) ->
      begin match lookup_symbol function_name with
      | None -> failwith ("Undefined function: " ^ function_name)
      | Some function_info ->
          (* Assuming the llvm_value for a function is a pointer to the function definition *)
          let function_pointer = function_info.llvm_value in
  
          (* Generate code for each argument *)
          let (arg_values, updated_symbol_table) = 
            List.fold_left (fun (values_acc, _) arg_expr -> 
              let (value, new_st) = codegen_expr {expr with node = arg_expr} in
              (value :: values_acc, new_st)
            ) ([], !symbol_table) arg_exprs in
  
          (* Make the function call *)
          let result = Llvm.build_call function_pointer (Array.of_list (List.rev arg_values)) "calltmp" builder in
          (result, updated_symbol_table)
            end
    | DIf (condition, then_block, else_block) ->
      let (cond_value, _st1) = codegen_expr {expr with node = condition} in
  
      (* Create basic blocks for then, else and merge *)
      let then_bb = Llvm.append_block context "then" (Llvm.block_parent (Llvm.insertion_block builder)) in
      let else_bb = Llvm.append_block context "else" (Llvm.block_parent (Llvm.insertion_block builder)) in
      let merge_bb = Llvm.append_block context "ifcont" (Llvm.block_parent (Llvm.insertion_block builder)) in
  
      (* Branch based on the condition value *)
      ignore (Llvm.build_cond_br cond_value then_bb else_bb builder);
  
      (* Then block *)
      Llvm.position_at_end then_bb builder;
      let _st2 = codegen_block then_block in
      ignore (Llvm.build_br merge_bb builder); (* Jump to merge after then block *)
  
      (* Else block *)
      Llvm.position_at_end else_bb builder;
      let st3 = codegen_block else_block in
      ignore (Llvm.build_br merge_bb builder); (* Jump to merge after else block *)
  
      (* Set builder's position to merge block for subsequent instructions *)
      Llvm.position_at_end merge_bb builder;
      (Llvm.const_null (Llvm.double_type context), st3)  (* Return dummy value, you might want to change this *)

  | DCreateThread (fname, args) ->
    (* Retrieve the function's LLVM value *)
    let func_ptr = lookup_function fname in
    (* Codegen the arguments *)
    let llvm_args = List.map (fun arg -> 
      let arg_dexpr = { loc = expr.loc; typ = expr.typ; node = arg; } in
      let (v, _) = codegen_expr arg_dexpr in 
      v) args in

      (* Prepare the pthread_create arguments *)
      let i8_lltype = i8_type context in
      let thread_ptr = build_alloca (pointer_type i8_lltype) "thread_ptr" builder in (* Allocated pointer to hold the thread id *)
      let attr = const_null (pointer_type i8_lltype) in (* null pointer for default attributes *)
      let start_routine = func_ptr the_module in
      let arg_for_start_routine = List.hd llvm_args in
      let pthread_create_function = lookup_function "create_thread" the_module in
      let info = { llvm_type = i8_lltype; llvm_value = thread_ptr } in
    
    (* Call pthread_create *)
      begin match pthread_create_function, start_routine with 
      | Some pthread_func, Some start_func -> 
        let ret_val = build_call pthread_func [| thread_ptr; attr; start_func; arg_for_start_routine |] "thread_create" builder in 
        let updated_symbol_table = add_to_current_scope "pthr_test" info in 
        symbol_table := updated_symbol_table;
        (ret_val, updated_symbol_table)
      | _ ->  failwith "Error: create_thread function not found in the module"
      end

  | DJoinThread thread_expr_node ->
    (* Codegen the thread identifier (which should give us the pointer to the pthread_t) *)
    let thread_expr = { loc = expr.loc; typ = expr.typ; node = thread_expr_node; } in
    let (thread_ptr_val, _) = codegen_expr thread_expr in
    
    (* Prepare for the pthread_join call *)
    let pthread_join_function = lookup_function "pthread_join" the_module in
    if pthread_join_function = None then failwith "Error: pthread_join function not found in the module";

    (* Call pthread_join *)
    let i8_lltype = i8_type context in
    let ret_val = match pthread_join_function with 
    | Some func -> 
        build_call func [| thread_ptr_val; const_null (pointer_type i8_lltype) |] "thread_join" builder 
    | None -> failwith "Error: pthread_join function not found in the module" in
    
    (ret_val, !symbol_table)

  | _ -> raise (Failure "not implemented")


(* ############################# Codegen Block ############################## *)
and codegen_block (block: dblock) : llvm_symbol_table =
  List.iter (fun expr -> 
    let _ = codegen_expr expr in
    ()
  ) block;
  !symbol_table

(* ############################# Codegen Struct ############################# *)
and codegen_struct (s: dstruct) : unit =
  (* Assuming a simple struct-to-LLVM mapping for now, like a tuple *)
  let llvm_field_types = List.map (fun (_, t) -> type_to_llvm t symbol_table) s.fields in
  let llvm_struct_type = Llvm.struct_type context (Array.of_list llvm_field_types) in

  (* Create a symbol_info for the struct *)
  let struct_info = {
    llvm_type = llvm_struct_type;
    llvm_value = Llvm.build_alloca llvm_struct_type s.name builder

  } in

  (* Add the struct type to the symbol table *)
  symbol_table := Symbol_table.add s.name struct_info !symbol_table

(* ############################ Codegen Function ############################ *)
and codegen_function_decl (f: dfunction) : unit =
    (* Convert function arguments to their LLVM types *)
    let llvm_arg_types = List.map (fun (t, _) -> type_to_llvm t symbol_table) f.params in
    let llvm_ret_type = type_to_llvm f.ret_type symbol_table in
    let llvm_func_type = Llvm.function_type llvm_ret_type (Array.of_list llvm_arg_types) in

    (* Declare the function *)
    let llvm_func = Llvm.declare_function f.name llvm_func_type the_module in

    (* Add function to the symbol table *)
    let func_info = { llvm_type = llvm_func_type; llvm_value = llvm_func } in
    symbol_table := Symbol_table.add f.name func_info !symbol_table
 
(* ########################### Type_expr -> lltype ########################### *)
and type_to_llvm (t: T.type_expr) (symbol_table: llvm_symbol_table ref) = 
  match t with
  | TEInt -> Llvm.i32_type context
  | TEBool -> Llvm.i1_type context
  | TEStruct sname -> 
      let name = A.Struct_name.to_string sname in
      (match Symbol_table.find_opt name !symbol_table with
      | Some info -> info.llvm_type
      | None -> failwith ("Unknown struct type: " ^ name))
  | TEVoid -> Llvm.void_type context
  end