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

  (* thread_create(pthread_t *thread, void *(*start_routine)(void *), void *arg) -> i32
     3 params matching the C wrapper in core_lib/bindings.c *)
  let fn_ptr_type = L.pointer_type (
    L.function_type (L.pointer_type (L.i8_type context)) [| L.pointer_type (L.i8_type context) |]
  ) in
  let thread_create_type =
    L.function_type (L.i32_type context) [| L.pointer_type pthread_t_type; fn_ptr_type; L.pointer_type (L.i8_type context) |] in
  let _ = L.declare_function "thread_create" thread_create_type the_module in

  (* thread_join(pthread_t thread, void **value_ptr) -> i32 *)
  let thread_join_type =
    L.function_type (L.i32_type context) [| pthread_t_type; L.pointer_type (L.pointer_type (L.i8_type context)) |] in
  let _ = L.declare_function "thread_join" thread_join_type the_module in

  (* pthread_equal(pthread_t t1, pthread_t t2) -> i32 *)
  let pthread_equal_type =
    L.function_type (L.i32_type context) [| pthread_t_type; pthread_t_type |] in
  let _ = L.declare_function "pthread_equal" pthread_equal_type the_module in

  (* pthread_self() -> pthread_t *)
  let pthread_self_type =
    L.function_type pthread_t_type [||] in
  let _ = L.declare_function "pthread_self" pthread_self_type the_module in

  ()

