(* open Poppy_parser
open Type_env
open Core
open Core.Result
open Core.Result.Let_syntax

let check_no_duplicate_method_signatures method_defns =
  let method_signatures = List.map ~f:(fun (Ast.TMethod (_, _, method_signatures, _)) -> method_signatures) method_defns in
  let method_signature_names = List.map ~f:(function Ast.TMethodSignature (method_name, _, _, _, _) -> method_name) method_signatures in
  if has_duplicates method_signature_names ~equal:Ast_types.Method_name.(=) then
    Or_error.errorf "%s Duplicate method names found" (Ast_types.Method_name.to_string (List.hd_exn method_signature_names))
  else
    Ok () 

let type_method_signature (Ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)) =
  Typed_ast.TMethodSignature(method_name, borrowed_ref, capabilities, params, ret_type)
    
let check_method_impl env trait_name struct_name =
  match env with
  | Global (struct_map, trait_map, _, _) ->
    let _ = StructNameMap.find struct_map struct_name in
    let _ = TraitNameMap.find trait_map trait_name in
    Ok ()
    (* check_method_impl_valid method_impl struct_defn trait_defn *)
  | _ -> Error (Base.Error.of_string "Method implementations should be checked in the global environment")
    

let check_method_signature_valid trait_defns (Ast.TMethodSignature(method_name, _, _, params, ret_type)) =
  let error_prefix = Fmt.str "Type error for method %s" (Ast_types.Method_name.to_string method_name) in
  (* Check that the return type is valid *)
  let%bind () = Type_env.check_type_valid trait_defns ret_type error_prefix in
  (* Check that the types of the parameters are valid *)
  let%bind () = Result.all_unit (List.map ~f:(fun (Ast_types.Param (param_type, _, _, _)) -> Type_env.check_type_valid trait_defns param_type error_prefix) params) in
  (* Check that the method signature matches the expected signature based on the trait and struct definitions *)
  (* let%bind () = check_signature_matches_trait_and_struct struct_defns trait_defns method_name params ret_type in *)
  Ok ()

  let init_env_from_method_params params env  =
    match env with
    | Global _ -> 
      let param_map = List.fold params ~init:VarNameMap.empty ~f:(fun map param ->
        let Ast_types.Param (_, param_name, _, _) = param in
        VarNameMap.add_exn map ~key:param_name ~data:param) in
      Ok (Function (env, param_map))
    | _ -> Error (Base.Error.of_string "Method parameters should be added in the global environment")

let type_method_defn struct_defns trait_defns (Ast.TMethod (trait_name, struct_name, method_signature, method_body) as method_defn) 
  function_defns env =
  let%bind () = check_method_signature_valid trait_defns method_signature in
  let%bind () = check_method_impl env trait_name struct_name in
  let typed_method_signature = type_method_signature method_signature in
  (* extract the params from the methods method_signature *)
  let params = match method_signature with 
    | Ast.TMethodSignature (_, _, _, params, _) -> params in
  let%bind new_env = init_env_from_method_params params env in
  let%bind typed_body_expr = Type_expr.type_block_expr struct_defns trait_defns [method_defn] 
    function_defns method_body new_env in
  Ok (Typed_ast.TMethod (trait_name, struct_name, typed_method_signature, typed_body_expr))
  
let type_method_defns struct_defns trait_defns method_defns function_defns env=
  Result.all (List.map ~f:(type_method_defn struct_defns trait_defns method_defns function_defns env) method_defns) *)
