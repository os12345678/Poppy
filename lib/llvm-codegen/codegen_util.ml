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

(* type exp = L.llvalue *)

let string_of_llmodule m =
  let s = string_of_llmodule m in
  dispose_module m;
  s

(* ############################# LLVM types ################################# *)
let get_llvm_type type_expr = 
  match type_expr with
| T.TEInt-> i32_type context
| T.TEBool -> i1_type context
| T.TEVoid -> void_type context
| T.TEStruct name -> named_struct_type context (T.Struct_name.to_string name)

(* ############################# LLVM values ################################# *)

let declare_thread_functions llmodule =
  (* Create the LLVM types for the functions *)

  (* pthread_t type (assuming it's i32) *)
  let pthread_t_type = Llvm.i32_type context in

  (* Type for thread function pointers. Assuming thread functions have 
     signature i32(i8*-). Adjust as needed. *)
  let thread_func_ptr_type = Llvm.pointer_type (Llvm.function_type (Llvm.i32_type context) [| Llvm.pointer_type (Llvm.i8_type context) |]) in

  (* Declare create_thread: pthread_t (i32(*)(i8*), i8*-) *)
  let _ = Llvm.declare_function "create_thread" (Llvm.function_type pthread_t_type [| thread_func_ptr_type; Llvm.pointer_type (Llvm.i8_type context) |]) llmodule in
  
  (* Declare join_thread: i32(pthread_t) *)
  let _ = Llvm.declare_function "join_thread" (Llvm.function_type (Llvm.i32_type context) [| pthread_t_type |]) llmodule in
  ()

(* ############################# Symbol Table ############################### *)

type symbol_info = {
  llvm_value: Llvm.llvalue;
  typ: T.type_expr;
}

module Symbol_table : sig
  type t

  val create : unit -> t
  val insert : t -> string -> Llvm.llvalue -> T.type_expr -> unit
  val lookup : t -> string -> symbol_info option
end = struct
  module StringHasher = struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
  end

  module SH = Hashtbl.Make(StringHasher)

  type t = symbol_info SH.t

  let create () = SH.create 16

  let insert table key llvm_val typ = 
    let info = { llvm_value = llvm_val; typ = typ } in
    SH.add table key info

  let lookup table key = SH.find_opt table key
end


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