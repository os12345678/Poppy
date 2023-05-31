open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
(* open Typed_ast *)

let type_identifier (_struct_defn: Ast.struct_defn list) (_function_defn: Ast.function_defn list) 
  (id: Ast.identifier) (ctx: context) (loc: loc) =
  let open Result in
  match id with
  | Ast.Variable var ->
    get_var_type ctx var loc
      >>| fun var_type -> (Typed_ast.TVariable (var, var_type), var_type)
  | Ast.ObjField (_var, _field_name) -> Or_error.error_string "ObjField not implemented"

let type_args type_expr_fn args context = 
  let open Result in 
  Result.all (List.map ~f: (fun expr -> type_expr_fn expr context) args)
  >>| fun typed_args_exprs_and_types -> List.unzip typed_args_exprs_and_types

let rec type_expr (struct_defns: Ast.struct_defn list) (interface_defns: Ast.interface_defn list) 
(function_defns: Ast.function_defn list) (expr: Ast.expr) context =
  let open Result in 
  let type_with_defns = type_expr struct_defns interface_defns function_defns in
  let _type_block_with_defns = type_block_expr struct_defns interface_defns function_defns in
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i}, TEInt)
  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b}, TEBool)
  | Ast.Identifier id ->
    type_identifier struct_defns function_defns id context expr.loc
    >>| fun (typed_id, id_type) -> ({Typed_ast.loc = expr.loc; typ = id_type; node = TIdentifier typed_id}, id_type)
  | Ast.Let (type_annot_maybe, var_name, expr)-> 
      is_this var_name expr.loc 
    >>= fun () ->
      type_with_defns expr context
      >>= fun (typed_expr, expr_type) ->
      let var_type = match type_annot_maybe with
        | Some type_annot -> type_annot
        | None -> expr_type
      in
      Ok ({Typed_ast.loc = expr.loc; typ = var_type; node = TLet (type_annot_maybe, var_name, typed_expr)}, var_type)
  | Ast.Assign (id, expr) -> 
      identifier_assignable id expr.loc
    >>= fun () ->
      type_with_defns expr context
    >>= fun (typed_expr, expr_type) ->
      type_identifier struct_defns function_defns id context expr.loc
    >>= fun (typed_id, id_type) ->
      if phys_equal id_type expr_type then
        Ok ({Typed_ast.loc = expr.loc; typ = id_type; node = TAssign (typed_id, typed_expr)}, id_type)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Trying to assign type %s to a field of type %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type id_type))
  | Ast.MethodApp (_, _, _) -> Or_error.error_string "Consult with Luke - how should they be typed"
  | Ast.FunctionApp (_func_name, _expr) -> Or_error.error_string "Not implemented"
    
  | _ -> Or_error.error_string "Not implemented"

and type_block_expr (struct_defns: Ast.struct_defn list) (interface_defns: Ast.interface_defn list) 
(function_defns: Ast.function_defn list) (Ast.Block (loc, exprs)) context =
  let open Result in 
  let type_with_defns = type_expr struct_defns interface_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns interface_defns function_defns in
  (* check_no_duplicate_var_declarations_in_block exprs loc
>>= fun () -> *)
  match exprs with 
  | [] -> Ok (Typed_ast.Block (loc, TEVoid, []), TEVoid)
  | [expr] ->
  type_with_defns expr context 
  >>| fun (typed_expr, expr_type) -> 
    (Typed_ast.Block (loc, expr_type, [typed_expr]), expr_type)
    | expr1 :: expr2 :: exprs ->
      type_with_defns expr1 context
      >>= fun (typed_expr1, expr1_type) ->
      (let updated_env =
         match typed_expr1.node with
         | TLet (_, var_name, _) -> 
          begin
            match add_variable context var_name expr1_type with
            | Ok updated_context -> updated_context
            | Error err -> failwith err
          end
          | _ -> context in
       type_block_with_defns (Ast.Block (loc, expr2 :: exprs)) updated_env)
      >>| fun (Typed_ast.Block (_, _, typed_exprs), block_expr_type) ->
      (Typed_ast.Block (loc, block_expr_type, typed_expr1 :: typed_exprs), block_expr_type)