module L = Llvm
module M = Codegen_util

let declare_externals the_module =
  (* Declare the print function *)
  let printf_type = 
    L.var_arg_function_type (L.void_type M.context) [| L.pointer_type (L.i8_type M.context) |] in
  let _ = L.declare_function "print" printf_type the_module in

  (* Declare the create_thread function *)
  let pthread_t_type = (* Assuming pthread_t is a pointer size, adjust if different *)
    L.pointer_type (L.i8_type M.context) in
  let start_routine_type = 
    L.pointer_type (L.function_type (L.pointer_type (L.i8_type M.context)) 
      [| L.pointer_type (L.i8_type M.context) |]) in
  let create_thread_type = 
    L.function_type pthread_t_type [| start_routine_type; L.pointer_type (L.i8_type M.context) |] in
  let _ = L.declare_function "create_thread" create_thread_type the_module in

  (* Declare the join_thread function *)
  let join_thread_type = 
    L.function_type (L.i32_type M.context) [| pthread_t_type |] in
  let _ = L.declare_function "join_thread" join_thread_type the_module in
  
  ()
