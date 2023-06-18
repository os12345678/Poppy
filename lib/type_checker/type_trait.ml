open Poppy_parser
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

(* Check for trait invariances *)

let check_no_duplicate_trait_names trait_defn =
  let trait_names = List.map ~f:(fun (Ast.TTrait (trait_name, _)) -> trait_name) trait_defn in
  if has_duplicates trait_names ~equal:Ast_types.Trait_name.(=)  then
    Or_error.errorf "Duplicate struct names found"
  else
    Ok ()

let check_no_duplicate_method_signatures trait_defn =
  let method_signature = List.map ~f:(fun (Ast.TTrait (_, method_signatures)) -> method_signatures) trait_defn in
  let method_signature_names = List.concat_map ~f:(List.map ~f:(function Ast.TMethodSignature (method_name, _, _, _, _) -> method_name)) method_signature in
  if has_duplicates method_signature_names ~equal:Ast_types.Method_name.(=)  then
    Or_error.errorf "Duplicate field names found"
  else
    Ok ()

(* Type check trait body *)

let init_env_from_params params =
  let param_pairs = List.map
    ~f:(function Ast_types.Param (type_expr, param_name, _, _) -> (param_name, type_expr))
    params in
  match VarNameMap.of_alist param_pairs with
  | `Duplicate_key _ -> Error (Base.Error.of_string "Duplicate parameter names found")
  | `Ok map -> Ok [map]

let type_method_type_sig trait_defns method_name params return_type = 
  let method_error_prefix =
    Fmt.str "Type error for method %s" (Ast_types.Method_name.to_string method_name) in
  let return_type_error_prefix = Fmt.str "%s return type" method_error_prefix in
  let%bind () = check_type_valid trait_defns return_type return_type_error_prefix in
  Result.all_unit  
  (List.map
  ~f:(fun (Ast_types.Param (param_type, param_name, _, _)) ->
    let param_error_prefix = 
      Fmt.str "%s param %s" return_type_error_prefix (Ast_types.Var_name.to_string param_name)
    in
    check_type_valid trait_defns param_type param_error_prefix)
  params)

let type_method_signature (Ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)) =
  Typed_ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)

  (* let type_method_defn 
  struct_defns trait_defns method_defns function_defns
  (Ast.TMethod (trait_name, struct_name, (Ast.TMethodSignature(method_name, _, _, params, ret_type) as method_sig), method_body)) = 
  let%bind () = type_method_type_sig trait_defns method_name params ret_type in
  let typed_method_sig = type_method_signature method_sig in
  let error_message = 
    Fmt.str
      "Type Error for method %s: expected return type of %s but got %s instead"
      (Ast_types.Method_name.to_string method_name)
      (Ast_types.string_of_type ret_type)
      (match typed_body_expr with
      | Typed_ast.Block (_,typed_exprs, _) -> 
        match typed_exprs with
        | _ -> Ast_types.string_of_type (typed_exprs)
      )
  in
  Ok (Typed_ast.TMethod (trait_name, struct_name, typed_method_sig, typed_body_expr))
  |> Result.map_error ~f:(fun _ -> Base.Error.of_string error_message) *)

  (* let type_method_defns struct_defns trait_defns method_defns function_defns method_defn=
    Result.all (List.map ~f:(type_method_defn struct_defns trait_defns method_defns function_defns) method_defn) *)

