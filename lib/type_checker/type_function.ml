(* open Poppy_parser
open Type_env
open Core 
open Core.Result
open Core.Result.Let_syntax

let init_env_from_params params =
  let param_pairs = List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params in
  match VarNameMap.of_alist param_pairs with
  | `Duplicate_key _ -> Error (Base.Error.of_string "Duplicate parameter names found")
  | `Ok map -> Ok [map]

let type_function_type_sig trait_defns func_name params return_type = 
  let function_error_prefix =
    Fmt.str "Type error for function %s" (Ast_types.Function_name.to_string func_name) in
  let return_type_error_prefix = Fmt.str "%s return type" function_error_prefix in
  let%bind () = check_type_valid trait_defns return_type return_type_error_prefix in
  Result.all_unit  
  (List.map
  ~f:(fun (Ast_types.Param (param_type, param_name, _, _)) ->
    let param_error_prefix = 
      Fmt.str "%s param %s" function_error_prefix (Ast_types.Var_name.to_string param_name)
    in
    check_type_valid trait_defns param_type param_error_prefix)
  params)
  
  let type_function_defn 
  struct_defns trait_defns method_defns function_defns
  (Ast.TFunction (function_name, borrowed_ref, return_type, params, block_expr)) = 
  let%bind () = type_function_type_sig trait_defns function_name params return_type in
  let%bind initial_context = init_env_from_params params in
  let%bind typed_body_expr = Type_expr.type_block_expr struct_defns trait_defns 
    method_defns function_defns block_expr initial_context in
  let error_message = 
    Fmt.str
      "Type Error for function %s: expected return type of %s but got %s instead"
      (Ast_types.Function_name.to_string function_name)
      (Ast_types.string_of_type return_type)
      (match typed_body_expr with
      | Typed_ast.Block (_,typed_exprs, _) -> 
        match typed_exprs with
        | _ -> Ast_types.string_of_type (typed_exprs)
      )
  in
  Ok (Typed_ast.TFunction (function_name, borrowed_ref, return_type, params, typed_body_expr))
  |> Result.map_error ~f:(fun _ -> Base.Error.of_string error_message)

  let type_function_defns struct_defns trait_defns method_defns function_defns =
    Result.all (List.map ~f:(type_function_defn struct_defns trait_defns method_defns function_defns) function_defns) *)