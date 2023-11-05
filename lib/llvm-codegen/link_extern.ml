module L = Llvm
module M = Codegen_util

let declare_externals the_module =
  let context = L.global_context () in  (* Assuming L is the LLVM module *)

  (* Declare the print function *)
  let printf_type = 
    L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |] in
  let _ = L.declare_function "print" printf_type the_module in

  (* Declare GC_malloc function *)
  let gc_malloc_type = 
    L.function_type (L.pointer_type (L.i8_type context)) [| L.i64_type context |] in
  let _ = L.declare_function "GC_malloc" gc_malloc_type the_module in

  (* Declare pthread-related functions *)
  let pthread_t_type = L.pointer_type (L.i8_type context) in
  let pthread_create_type = 
    L.function_type (L.i32_type context) [| L.pointer_type pthread_t_type; L.pointer_type (L.i8_type context); L.pointer_type (L.i8_type context); L.pointer_type (L.i8_type context) |] in
  let _ = L.declare_function "thread_create" pthread_create_type the_module in

  let pthread_join_type = 
    L.function_type (L.i32_type context) [| pthread_t_type; L.pointer_type (L.pointer_type (L.i8_type context)) |] in
  let _ = L.declare_function "thread_join" pthread_join_type the_module in

  let pthread_equal_type = 
    L.function_type (L.i32_type context) [| pthread_t_type; pthread_t_type |] in
  let _ = L.declare_function "pthread_equal" pthread_equal_type the_module in

  let pthread_self_type = 
    L.function_type pthread_t_type [||] in  (* Empty array for no arguments *)
  let _ = L.declare_function "pthread_self" pthread_self_type the_module in

  ()

