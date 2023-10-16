open Core
open Poppy_type_checker.Type_env
open Poppy_type_checker.Typed_ast

open Type_data_race_expr
open Type_data_race_function

let type_data_race_program (env: env)
(Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) : (program, Error.t) Result.t = 
  let open Result in
    Result.all
    (List.map
      ~f:(fun function_defn -> type_data_races_function_defn env function_defn)
      function_defns)
    >>= fun data_race_checked_function_defns ->
      type_data_races_block_expr env main_expr [] 
      >>| fun data_race_checked_main_expr ->
        (Prog (struct_defns, trait_defns, method_defns, data_race_checked_function_defns, data_race_checked_main_expr))
