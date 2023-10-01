let context = Llvm.global_context ()
let the_module = Llvm.create_module context "Poppy JIT"
let builder = Llvm.builder context

let active_threads_table : (Llvm.llvalue, bool) Hashtbl.t = Hashtbl.create 50


let handle_string_constant value =
  print_endline ("Handling string constant " ^ (Llvm.string_of_llvalue value));
  let string_ptr_type = Llvm.pointer_type (Llvm.i8_type context) in
  let casted_ptr = Llvm.build_bitcast value string_ptr_type "casted_str_ptr" builder in
  print_endline ("Resulting value: " ^ (Llvm.string_of_llvalue casted_ptr));
  casted_ptr

let is_valid_thread_id thread_id = 
  try 
      Hashtbl.find active_threads_table thread_id 
  with Not_found -> false

let clear_thread_id thread_id = 
  Hashtbl.remove active_threads_table thread_id