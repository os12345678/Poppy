(* open Core

module T = Poppy_parser.Ast_types
module A = Poppy_type_checker.Typed_ast
module E = Data_race_env

(* filter capabilities that are themselves thread-local or subord or share state that are
   not safe() with thread or subord capabilities *)
let filter_capabilities_with_thread_state struct_name env
   all_capabilities curr_capability =
 let thread_or_subord_capabilities =
   List.filter
     ~f:(fun capability ->
       E.capability_fields_have_mode capability struct_name T.ThreadLocal env)
     all_capabilities in
 (* check we can concurrently access this capability with the thread or subord
    capabilities, i.e all overlapping state is safe() *)
 List.for_all
   ~f:(fun capability ->
     can_concurrently_access_capabilities class_name class_defns capability
       curr_capability)
   thread_or_subord_capabilities

   let remove_thread_caps_from_async_expr class_defns
   (A.AsyncExpr (free_var_types_and_capabilities, async_expr)) =
 (* update async expression if the free variable contains thread-local or subord state -
    remove those capabilities from all aliases *)
 let updated_async_expr =
   List.fold ~init:async_expr
     ~f:(fun acc_async_expr (obj_name, obj_class, _) ->
       let thread_obj =
         class_has_mode obj_class ThreadLocal class_defns in
       if thread_obj then
         let aliases_to_remove_thread_caps =
           find_aliases_in_block_expr ~should_match_fields:true obj_name acc_async_expr
           (* match fields since if x is not thread-local, x.f isn't *) in
         update_matching_identifier_caps_block_expr
           (obj_name :: aliases_to_remove_subord_thread_caps)
           (filter_capabilities_with_thread_state obj_class class_defns)
           acc_async_expr
       else acc_async_expr)
     free_var_types_and_capabilities in
 AsyncExpr (free_var_types_and_capabilities, updated_async_expr) *)