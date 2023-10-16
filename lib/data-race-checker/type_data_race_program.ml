open Core
open Type_data_race_expr

module T = Poppy_type_checker.Typed_ast
(* module E = Data_race_checker.Type_data_race_expr *)

let type_data_race_program
(T.Prog (struct_defns, trait_defns, method_defns, function_defns, main_expr)) : (T.program, Error.t) Result.t = 
  let open Result in
  match type_data_races_block_expr env main_expr [] with
  | Ok data_race_checked_main_expr -> 
    Ok (T.Prog (struct_defns, trait_defns, method_defns, function_defns, data_race_checked_main_expr))
  | Error err -> 
    Error err
