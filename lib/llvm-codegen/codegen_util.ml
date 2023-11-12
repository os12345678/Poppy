(* open Llvm_executionengine
open Llvm_target *)

open Llvm

module D = Desugar.Desugared_ast
module T = Poppy_parser.Ast_types
module St = Ir_symbol_table

let context = global_context ()
let the_module = create_module context "Poppy JIT"
let builder = builder context

(* let the_execution_engine = Llvm_executionengine.create the_module in
let the_fpm = PassManager.create_function the_module *)

let active_threads_table : (string, llvalue list) Hashtbl.t = Hashtbl.create 50

let wrap loc typ node = {
  D.loc = loc;
  D.typ = typ;
  D.node = node;
}

let llvm_type_of_typ = function
  | T.TEInt -> i32_type context
  | T.TEBool -> i1_type context
  | T.TEVoid -> void_type context
  | T.TEStruct s ->  match type_by_name the_module (T.Struct_name.to_string s) with
  | None -> failwith ("Struct " ^ (T.Struct_name.to_string s) ^ " not found")
  | Some llvm_type -> llvm_type

let handle_string_constant value =
  let string_ptr_type = pointer_type (i8_type context) in
  let casted_ptr = build_bitcast value string_ptr_type "casted_str_ptr" builder in
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
  Hashtbl.iter (fun key value -> print_endline ("Key: " ^ key ^ " Value: " ^ (string_of_llvalue (List.hd value)))) active_threads_table

let generate_wrapper_function thread_name arg_types return_type =
   (* Define the wrapper function type *)
   let wrapper_type = function_type (pointer_type return_type)
   [| pointer_type (i8_type context) |] in
   let wrapper_fn = define_function (thread_name ^ "_wrapper") wrapper_type the_module in

   (* Create a new builder for the wrapper function *)
   let wrapper_builder = builder_at_end context (entry_block wrapper_fn) in

    (* Look up the original function *)
    let original_fn = match lookup_function thread_name the_module with
                      | Some fn -> fn
                      | None -> failwith "Original function not found in the LLVM module." in

  (* Unpack the arguments *)
  let unpacked_args = List.mapi (fun i typ ->
    let gep = build_gep (param wrapper_fn 0) [| const_int (i32_type context) i |] "" wrapper_builder in
    let casted_ptr = build_bitcast gep (pointer_type typ) "" wrapper_builder in
    build_load casted_ptr "" wrapper_builder
  ) arg_types in

  (* Call the original function *)
  let result = build_call original_fn (Array.of_list unpacked_args) "" wrapper_builder in

  (* Pack the result *)
  let packed_result = build_malloc return_type "" wrapper_builder in
  ignore (build_store result packed_result wrapper_builder);

  (* Cast the result to i8* and return it *)
  let result_as_i8_ptr = build_bitcast packed_result (pointer_type (i8_type context)) "" wrapper_builder in
  ignore (build_ret result_as_i8_ptr wrapper_builder);

  wrapper_fn  
  
  let pack_args args arg_types =
    let packed_space = build_malloc (i8_type context) (string_of_int (List.length args)) builder in
    
    List.iteri (fun i (arg, typ) ->
        let gep = build_gep packed_space [| const_int (i32_type context) i |] "" builder in
        let casted_ptr = build_bitcast gep (pointer_type typ) "" builder in
        ignore (build_store arg casted_ptr builder);
    ) (List.combine args arg_types);
    
    packed_space


let adjust_arg_type expected_type arg_value =
  match classify_type expected_type, classify_type (type_of arg_value) with
  | TypeKind.Struct, TypeKind.Pointer when Core.phys_equal expected_type (element_type (type_of arg_value)) ->
      (* If the function expects a struct and we have a pointer to that struct, dereference the pointer *)
      build_load arg_value "" builder
  | _ ->
      (* Otherwise, leave the argument as it is *)
      arg_value

let is_string_literal value =
  match classify_value value with
  | ConstantArray -> true
  | _ -> false

  let define_global_string str_val the_module context =
    (* let lltype = Llvm.array_type (Llvm.i8_type context) (String.length str_val + 1) in *)
    let str_llval = Llvm.const_stringz context str_val in
    let global_var = Llvm.define_global "str" str_llval the_module in
    Llvm.set_linkage Private global_var;
    Llvm.set_global_constant true global_var;
    Llvm.set_initializer str_llval global_var;
    global_var