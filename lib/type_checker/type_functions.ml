(* open Poppy_parser
open Type_env
open Core 

let init_env_from_params params =
  List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params

let rec type_check_function function_defns func_name expr_loc args context = 
  let open Result in
  match get_function_defn function_defns func_name with
  | Some function_defn ->
      (* Create a new scope for this function, initially containing the types of the parameters *)
      let initial_scope = init_env_from_params function_defn.params in
      let context = initial_scope :: context in
      (* Type check the function arguments *)
      type_check_args function_defn args context
      (* Check if the return type of the function matches the expected type *)
      >>= fun arg_types -> type_check_return_type function_defn arg_types expr_loc context
  | None -> Or_error.error_string (Fmt.str "%s Undefined function: %s" (string_of_loc expr_loc) func_name)
    
and type_check_args function_defn args context = 
  let open Result in
  let expected_arg_types = function_defn.arg_types in
  if List.length expected_arg_types != List.length args then
    Or_error.error_string (Fmt.str "%s Type error - Incorrect number of arguments for function %s" 
                            (string_of_loc expr_loc) function_defn.name)
  else
    Result.all (List.map ~f: (type_expr args context))
    >>= fun actual_arg_types ->
    if List.equal ~equal:phys_equal expected_arg_types actual_arg_types then
      Ok actual_arg_types
    else
      Or_error.error_string (Fmt.str "%s Type error - Function arguments do not match function type: %s" 
                              (string_of_loc expr_loc) (string_of_type expected_arg_types))


and type_check_return_type function_defn arg_types expr_loc = 
  let expected_return_type = function_defn.return_type in
  (* TODO: Type check the body of the function *)
  type_check_body function_defn.body arg_types expected_return_type expr_loc
  >>= fun actual_return_type ->
  if phys_equal expected_return_type actual_return_type then
    Ok function_defn.return_type
  else
    Or_error.error_string (Fmt.str "%s Type error - Function return type does not match expected return type: %s" 
                            (string_of_loc expr_loc) (string_of_type expected_return_type))
 *)
