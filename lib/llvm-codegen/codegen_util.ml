(* open Llvm_executionengine
open Llvm_target *)

module D = Desugar.Desugared_ast
module T = Poppy_parser.Ast_types

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "Poppy JIT"
let builder = Llvm.builder context

(* let the_execution_engine = Llvm_executionengine.create the_module in
let the_fpm = Llvm.PassManager.create_function the_module *)

let active_threads_table : (string, Llvm.llvalue list) Hashtbl.t = Hashtbl.create 50

let wrap loc typ node = {
  D.loc = loc;
  D.typ = typ;
  D.node = node;
}

let llvm_type_of_typ = function
  | T.TEInt -> Llvm.i32_type context
  | T.TEBool -> Llvm.i1_type context
  | T.TEVoid -> Llvm.void_type context
  | T.TEStruct s -> Llvm.named_struct_type context (T.Struct_name.to_string s)

let handle_string_constant value =
  let string_ptr_type = Llvm.pointer_type (Llvm.i8_type context) in
  let casted_ptr = Llvm.build_bitcast value string_ptr_type "casted_str_ptr" builder in
  casted_ptr

let is_valid_thread_id thread_id = 
  try 
      Hashtbl.find active_threads_table thread_id 
  with Not_found -> []

let clear_thread_id thread_id = 
  Hashtbl.remove active_threads_table thread_id

let check_and_remove_thread_from_table fname =
  match Hashtbl.find_opt active_threads_table fname with
  | Some (thread_id_value::remaining_threads) -> 
      (* For simplicity, let's consider only taking the first one. *)
      Hashtbl.replace active_threads_table fname remaining_threads;
      Some thread_id_value
  | Some [] | None -> 
      None

let add_thread_to_table fname thread_id_value =
  let current_threads = 
      match Hashtbl.find_opt active_threads_table fname with
      | Some threads -> threads
      | None -> [] 
  in
  Hashtbl.replace active_threads_table fname (thread_id_value :: current_threads)
    
let print_hash_table () = 
  Hashtbl.iter (fun key value -> print_endline ("Key: " ^ key ^ " Value: " ^ (Llvm.string_of_llvalue (List.hd value)))) active_threads_table

  let generate_wrapper_function fname arg_types return_type =
    let wrapper_type = Llvm.function_type (Llvm.pointer_type (Llvm.i8_type context)) [| Llvm.pointer_type (Llvm.i8_type context) |] in
    let wrapper_fn = Llvm.define_function (fname ^ "_wrapper") wrapper_type the_module in
  
    let builder = Llvm.builder_at_end context (Llvm.entry_block wrapper_fn) in
  
    let original_fn = Llvm.lookup_function fname the_module in
  
    let unpacked_args = List.mapi (fun i typ ->
        let gep = Llvm.build_gep (Llvm.param wrapper_fn 0) [| Llvm.const_int (Llvm.i32_type context) i |] "" builder in
        let casted_ptr = Llvm.build_bitcast gep (Llvm.pointer_type typ) "" builder in
        Llvm.build_load casted_ptr "" builder
    ) arg_types in
  
    let result = Llvm.build_call (Option.get original_fn) (Array.of_list unpacked_args) "" builder in
  
    let packed_result = Llvm.build_malloc return_type "" builder in
    ignore (Llvm.build_store result packed_result builder);
  
    let result_as_i8_ptr = Llvm.build_bitcast packed_result (Llvm.pointer_type (Llvm.i8_type context)) "" builder in
  
    ignore (Llvm.build_ret result_as_i8_ptr builder);
  
    wrapper_fn
  
  let pack_args args arg_types =
    let packed_space = Llvm.build_malloc (Llvm.i8_type context) (string_of_int (List.length args)) builder in
    
    List.iteri (fun i (arg, typ) ->
        let gep = Llvm.build_gep packed_space [| Llvm.const_int (Llvm.i32_type context) i |] "" builder in
        let casted_ptr = Llvm.build_bitcast gep (Llvm.pointer_type typ) "" builder in
        ignore (Llvm.build_store arg casted_ptr builder);
    ) (List.combine args arg_types);
    
    packed_space