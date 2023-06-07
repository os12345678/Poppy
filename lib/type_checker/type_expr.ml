open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
(* open Type_functions *)
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

let rec type_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (method_defns: Ast.method_defn list)
(function_defns: Ast.function_defn list) (expr: Ast.expr) context =
  let open Result in 
  let type_with_defns = type_expr struct_defns trait_defns method_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns trait_defns method_defns function_defns in
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i}, TEInt)
  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b}, TEBool)
  | Ast.Identifier id ->
    type_identifier struct_defns function_defns id context expr.loc
    >>| fun (typed_id, id_type) -> ({Typed_ast.loc = expr.loc; typ = id_type; node = TIdentifier typed_id}, id_type)
  | Ast.Let (type_annot_maybe, var_name, let_expr)-> 
      is_this var_name expr.loc 
    >>= fun () ->
      type_with_defns let_expr context
      >>= fun (typed_expr, let_expr_type) ->
      let var_type = match type_annot_maybe with
        | Some type_annot -> if phys_equal type_annot let_expr_type then type_annot
          else failwith (Fmt.str "%s Type error - Let expression type %s does not match type annotation %s" 
            (string_of_loc expr.loc) (string_of_type let_expr_type) (string_of_type type_annot))
        | None -> let_expr_type
      in
      Ok ({Typed_ast.loc = expr.loc; typ = var_type; node = TLet (type_annot_maybe, var_name, typed_expr)}, var_type)
  | Ast.Constructor(_,_,_) -> Or_error.error_string "Constructor Not implemented"
  | Ast.Assign (id, assignable_expr) -> 
      identifier_assignable id expr.loc
    >>= fun () ->
      type_with_defns assignable_expr context
    >>= fun (typed_expr, expr_type) ->
      type_identifier struct_defns function_defns id context expr.loc
    >>= fun (typed_id, id_type) ->
      if phys_equal id_type expr_type then
        Ok ({Typed_ast.loc = expr.loc; typ = id_type; node = TAssign (typed_id, typed_expr)}, id_type)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Trying to assign type %s to a field of type %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type id_type))
  | Ast.MethodApp (_,_,_) -> Or_error.error_string "MethodApp Not implemented"
  | Ast.FunctionApp(_,_) -> Or_error.error_string "MethodApp Not implemented"
  (* | Ast.FunctionApp (func_name,arg_expr) -> 
    type_args type_with_defns arg_expr context
    >>= fun (typed_arg_exprs, arg_expr_types) ->
    get_function_type function_defns func_name expr.loc
    >>= fun (func_type, return_type) ->
    if phys_equal arg_expr_types func_type then
      Ok ({Typed_ast.loc = expr.loc; typ = return_type; node = TFunctionApp (func_name, typed_arg_exprs)}, return_type)
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - Function arguments do not match function type: %s" 
        (string_of_loc expr.loc) (string_of_type func_type)) *)
  | Ast.FinishAsync (_,_,_) -> Or_error.error_string "FinishAsync Not implemented"
  | Ast.If (cond, then_expr, else_expr) ->
    type_with_defns cond context
    >>= fun (typed_cond, cond_type) ->
    type_block_with_defns then_expr context
    >>= fun (typed_then_expr, then_expr_type) ->
    type_block_with_defns else_expr context
    >>= fun (typed_else_expr, else_expr_type) ->
    if phys_equal cond_type TEBool then
      if phys_equal then_expr_type else_expr_type then
        Ok ({Typed_ast.loc = expr.loc; typ = then_expr_type; node = TIf (typed_cond, typed_then_expr, typed_else_expr)}, then_expr_type)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - If statement branches have different types: %s and %s" 
          (string_of_loc expr.loc) (string_of_type then_expr_type) (string_of_type else_expr_type))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - If statement condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type cond_type))
  | Ast.While (cond_expr, block_expr) ->
    type_with_defns cond_expr context
    >>= fun (typed_cond_expr, cond_type) ->
    type_block_with_defns block_expr context
    >>= fun (typed_block_expr, _) ->
    if phys_equal cond_type TEBool then
      Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TWhile (typed_cond_expr, typed_block_expr)}, TEVoid)
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - While loop condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type cond_type))
  | Ast.For (start_expr, cond_expr, step_expr, block_expr) ->
    type_with_defns start_expr context
    >>= fun (typed_start_expr, start_expr_type) ->
    type_with_defns cond_expr context
    >>= fun (typed_cond_expr, cond_expr_type) ->
    type_with_defns step_expr context
    >>= fun (typed_step_expr, step_expr_type) ->
    type_block_with_defns block_expr context
    >>= fun (typed_block_expr, _) ->
    if phys_equal cond_expr_type TEBool then 
      if phys_equal start_expr_type TEInt && phys_equal step_expr_type TEInt then
        Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TFor (typed_start_expr, typed_cond_expr, typed_step_expr, typed_block_expr)}, TEVoid)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - For loop start and step expressions must be integers: %s and %s" 
          (string_of_loc expr.loc) (string_of_type start_expr_type) (string_of_type step_expr_type))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - For loop condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type cond_expr_type))
  | Ast.BinOp (binop, lhs, rhs) ->
    type_with_defns lhs context
    >>= fun (typed_lhs, lhs_type) ->
    type_with_defns rhs context
    >>= fun (typed_rhs, rhs_type) ->
    if phys_equal lhs_type rhs_type then 
      match binop with
      | BinOpPlus | BinOpMinus | BinOpMult | BinOpIntDiv | BinOpRem ->
        if phys_equal lhs_type TEInt then
          Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TBinOp (binop, typed_lhs, typed_rhs)}, TEInt)
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Arithmetic operations can only be performed on integers: %s" 
            (string_of_loc expr.loc) (string_of_type lhs_type))
      | BinOpEq | BinOpNotEq | BinOpLessThan | BinOpLessThanEq | BinOpGreaterThan | BinOpGreaterThanEq ->
        if phys_equal lhs_type TEInt || phys_equal lhs_type TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)}, TEBool)
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Comparison operations can only be performed on integers and booleans: %s" 
            (string_of_loc expr.loc) (string_of_type lhs_type))
      | BinOpAnd | BinOpOr ->
        if phys_equal lhs_type TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)}, TEBool)
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Logical operations can only be performed on booleans: %s" 
            (string_of_loc expr.loc) (string_of_type lhs_type))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - Binary operation operands must have the same type: %s and %s" 
        (string_of_loc expr.loc) (string_of_type lhs_type) (string_of_type rhs_type))
  (* | Ast.UnOp (unop, unop_expr) ->
    type_with_defns unop_expr context
    >>= fun (typed_unop_expr, unop_expr_type) ->  
    match unop with
    | UnOpNeg -> if phys_equal unop_expr_type TEInt then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TUnOp (unop, typed_unop_expr)}, TEInt)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be an integer: %s" 
          (string_of_loc expr.loc) (string_of_type unop_expr_type))
    | UnOpNot -> if phys_equal unop_expr_type TEBool then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TUnOp (unop, typed_unop_expr)}, TEBool)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be a boolean: %s" 
          (string_of_loc expr.loc) (string_of_type unop_expr_type)) *)
  | Ast.UnOp (_,_) -> Or_error.error_string "UnOp Not implemented"
  (* | Ast.NewStruct (_,_) -> Or_error.error_string "NewStruct Not implemented" *)
  (* | Ast.AssignToInterface (_,_) -> Or_error.error_string "AssignToInterface Not implemented" *)

and type_block_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (method_defns: Ast.method_defn list)
(function_defns: Ast.function_defn list) (Ast.Block (loc, exprs)) context =
  let open Result in 
  let type_with_defns = type_expr struct_defns trait_defns method_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns trait_defns method_defns function_defns in
  let new_context = Type_env.push_scope context in
  print_endline ("new context: ");
  print_context new_context;
  print_endline ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  (* check_no_duplicate_var_declarations_in_block exprs loc
>>= fun () -> *)
  match exprs with 
  | [] -> Ok (Typed_ast.Block (loc, TEVoid, []), TEVoid)
  | [expr] ->
  type_with_defns expr new_context 
  >>| fun (typed_expr, expr_type) -> 
    (Typed_ast.Block (loc, expr_type, [typed_expr]), expr_type)
  | expr1 :: expr2 :: exprs ->
    type_with_defns expr1 new_context
    >>= fun (typed_expr1, expr1_type) ->
    (let updated_env =
        match typed_expr1.node with
        | TLet (_, var_name, _) -> 
        begin
          match add_variable new_context var_name expr1_type with
          | Ok updated_context -> updated_context
          | Error err -> failwith err
        end
        | _ -> new_context in
      type_block_with_defns (Ast.Block (loc, expr2 :: exprs)) updated_env)
    >>| fun (Typed_ast.Block (_, _, typed_exprs), block_expr_type) -> 
      (Typed_ast.Block (loc, block_expr_type, typed_expr1 :: typed_exprs), block_expr_type) 