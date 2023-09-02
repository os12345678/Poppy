open Llvm
open Poppy_parser
(* open Ast_types *)

(* ############################# LLVM setup ################################# *)
module L = Llvm
module T = Ast_types
let context = global_context ()
let the_module = create_module context "Poppy_JIT"
let builder = builder context

type break_block = Llvm.llbasicblock

type exp = L.llvalue

let string_of_llmodule m =
  let s = string_of_llmodule m in
  dispose_module m;
  s

(* ############################# LLVM types ################################# *)
let get_llvm_type type_expr = 
  match type_expr with
| T.TEInt-> i64_type context
| T.TEBool -> i1_type context
| T.TEVoid -> void_type context
| T.TEStruct name -> named_struct_type context (T.Struct_name.to_string name)
| _ -> raise (Failure "Not implemented yet")
(* | TELocked _ -> i64_type context
| TEUnlocked _ -> i64_type context *)

(* ########################### Frame Pointer ############################### *)
let int_exp i = L.const_int (i32_type context) i
let malloc (name: string) (typ: T.type_expr): exp =
  L.build_malloc (typ |> get_llvm_type) name builder 

let frame_pointer_stack = Stack.create()

let push_fp_to_stack (typ: L.lltype) (addr: L.llvalue) =
  Stack.push (typ, addr) frame_pointer_stack

let pop_fp_from_stack () = Stack.pop frame_pointer_stack

let get_current_fp () = Stack.top frame_pointer_stack

let get_fp_value () : llvalue =
  let (_, fp_struct_addr) = get_current_fp() in
  let fp_addr = L.build_gep fp_struct_addr [| int_exp(0); int_exp (0) |] "frame_pointer_address" builder in
  L.build_load fp_addr "frame_pointer_val" builder  


(* ############################# LLVM values ################################# *)


(* ################### Helper Functions for Aux Functions ################### *)


(* ########################### Core Library ################################# *)
let link_core_library the_module =
  let bindings = "/Users/oliver/Documents/University/Honours/poppy/core_lib/bindings.ll" in

  (* Parse the core library LLVM IR *)
  let context = global_context () in
  let corelib_buf = MemoryBuffer.of_file bindings in
  let corelib_module =
    try
      Llvm_irreader.parse_ir context corelib_buf
    with
    | Llvm_irreader.Error msg ->
      Printf.printf "Error parsing core library: %s\n" msg;
      raise (Failure "Error parsing core library")
    in
  Llvm.iter_functions (fun f -> Printf.printf "Core lib function: %s\n" (Llvm.value_name f)) corelib_module;

  (* Link the core library into the main module *)
  Llvm_linker.link_modules' the_module corelib_module;
  print_endline "Core library linked to the main module."