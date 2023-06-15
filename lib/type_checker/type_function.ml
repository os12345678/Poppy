open Poppy_parser
open Type_env
open Core 

let equal_type_expr t1 t2 =
  match t1, t2 with
  | Ast_types.TEInt, Ast_types.TEInt -> true
  | Ast_types.TEBool, Ast_types.TEBool -> true
  | Ast_types.TEVoid, Ast_types.TEVoid -> true
  | Ast_types.TEStruct struct_name1, Ast_types.TEStruct struct_name2 ->
    String.equal (Ast_types.Struct_name.to_string struct_name1) (Ast_types.Struct_name.to_string struct_name2) 
  | Ast_types.TETrait trait_name1, Ast_types.TETrait trait_name2 ->
    String.equal (Ast_types.Trait_name.to_string trait_name1) (Ast_types.Trait_name.to_string trait_name2)
  | _, _ -> false


let init_env_from_params params =
  let param_pairs = List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params in
  match VarNameMap.of_alist param_pairs with
  | `Duplicate_key _ -> Error (Base.Error.of_string "Duplicate parameter names found")
  | `Ok map -> Ok [map]

let check_no_duplicate_function_names function_defn =
  let function_names = List.map ~f:(fun (Ast.TFunction (function_name, _, _, _, _)) -> function_name) function_defn in
  if has_duplicates function_names ~equal:Ast_types.Function_name.(=)  then
    Or_error.errorf "Duplicate function names found"
  else
    Ok ()

let check_valid_function_parameters function_defn struct_defns =
  let params_lists = List.map ~f:(fun (Ast.TFunction (_, _, _, params, _)) -> params) function_defn in
  let params_type_exprs = List.concat_map ~f:(List.map ~f:(function Ast_types.Param (type_expr, _, _, _) -> type_expr)) params_lists in
  List.map ~f:(check_type_valid struct_defns) params_type_exprs
  |> Result.all_unit

let check_valid_return_type function_defn struct_defns =
  let return_types = List.map ~f:(fun (Ast.TFunction (_, _, return_type, _, _)) -> return_type) function_defn in
  List.map ~f:(check_type_valid struct_defns) return_types
  |> Result.all_unit
  
  let type_function_defn 
  struct_defns trait_defns method_defns function_defns
  (Ast.TFunction (function_name, borrowed_ref, return_type, params, block_expr) as current_function_defn) = 
  let open Result in
  let open Core.Result.Let_syntax in
  let%bind () = check_no_duplicate_function_names [current_function_defn] in
  let%bind () = check_valid_function_parameters [current_function_defn] struct_defns in
  let%bind () = check_valid_return_type [current_function_defn] struct_defns in
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
    Result.all (List.map ~f:(type_function_defn struct_defns trait_defns method_defns function_defns) function_defns)