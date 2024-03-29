open Core
(* open Poppy_type_checker.Type_env *)
(* open Poppy_type_checker.Typed_ast *)

open Type_data_race_expr
open Type_data_race_function
open Type_data_race_method

module E = Poppy_type_checker.Type_env
module T = Poppy_type_checker.Typed_ast

let flatten_method_defns impl_defns =
  List.fold_left impl_defns ~init:[] ~f:(fun acc (T.TImpl (_, _, methods)) ->
    acc @ methods
  )
let type_data_race_program (env: E.env)
(T.Prog (struct_defns, trait_defns, impl_defns, function_defns, main_expr)) ~ignore_data_races = 
  let open Result in
    Result.all (List.map ~f:(fun (TImpl (trait_name, struct_name, methods)) ->
      Result.all (List.map ~f:(type_data_races_method_defn struct_name trait_name env struct_defns trait_defns impl_defns (flatten_method_defns impl_defns) function_defns ~ignore_data_races) methods)
      >>| fun checked_methods -> T.TImpl (trait_name, struct_name, checked_methods))
    impl_defns) 
    >>= fun data_race_checked_impl_defns ->
    Result.all
    (List.map
      ~f:(fun function_defn -> type_data_races_function_defn struct_defns trait_defns impl_defns (flatten_method_defns impl_defns) function_defns function_defn env ~ignore_data_races)
      function_defns)
    >>= fun data_race_checked_function_defns ->
      type_data_races_block_expr struct_defns trait_defns impl_defns (flatten_method_defns impl_defns) function_defns env main_expr [] ~ignore_data_races
      >>| fun data_race_checked_main_expr ->
        (T.Prog (struct_defns, trait_defns, data_race_checked_impl_defns, data_race_checked_function_defns, data_race_checked_main_expr))