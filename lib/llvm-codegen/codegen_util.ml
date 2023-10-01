let context = Llvm.global_context ()
let the_module = Llvm.create_module context "Poppy JIT"
let builder = Llvm.builder context

let active_threads_table : (string, Llvm.llvalue list) Hashtbl.t = Hashtbl.create 50

let handle_string_constant value =
  print_endline ("Handling string constant " ^ (Llvm.string_of_llvalue value));
  let string_ptr_type = Llvm.pointer_type (Llvm.i8_type context) in
  let casted_ptr = Llvm.build_bitcast value string_ptr_type "casted_str_ptr" builder in
  print_endline ("Resulting value: " ^ (Llvm.string_of_llvalue casted_ptr));
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