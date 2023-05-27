open Poppy_parser
open Type_env
open Core 

let init_env_from_params params =
  List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params

