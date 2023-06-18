open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
open Core.Result
open Core.Result.Let_syntax
(* open Type_functions *)
(* open Typed_ast *)

let type_identifier struct_defns _function_defns
  (id: Ast.identifier) context loc =
  match id with
  | Ast.Variable var ->
    get_var_type context var loc
      >>| fun var_type -> (Typed_ast.TVariable (var, var_type), var_type)
  | Ast.ObjField (var_name, field_name) -> 
    get_obj_struct_defn var_name struct_defns context loc 
    >>= fun struct_defn ->
      get_struct_field field_name struct_defns struct_defn loc
      >>| fun field_type -> 
        (Typed_ast.TObjField (var_name, field_name, field_type), field_type)

let type_args type_expr_fn args env =
  Result.all (List.map ~f:(fun expr -> type_expr_fn expr env) args)

let type_constructor_args struct_defn struct_name constructor_args 
(type_expr_fn: Ast.expr -> context -> (Typed_ast.expr, Base.Error.t) Result.t) loc context =
match struct_defn with
| Ast.TStruct (_, _, fields) ->
  let rec check_args args field_defs =
    match args, field_defs with
    | [], [] -> Ok []
    | (Ast.ConstructorArg(field_name, expr))::arg_t, (TField (_, type_of_field, name, _))::field_t when Field_name.(=) field_name name ->
      type_expr_fn expr context
      >>= fun (typed_expr) ->
      if phys_equal typed_expr.typ type_of_field then 
        check_args arg_t field_t
        >>= fun remaining ->
        Ok (Typed_ast.ConstructorArg(field_name, typed_expr)::remaining)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Constructor argument type %s does not match the expected type %s for field %s" 
          (string_of_loc loc) (string_of_type typed_expr.typ) (string_of_type type_of_field) (Field_name.to_string field_name))
    | _, _ -> Or_error.error_string (Fmt.str "%s Type error - # of constructor arguments do not match with # of struct fields for %s. Expected %s but got %s" 
          (string_of_loc loc) (Struct_name.to_string struct_name) (List.length fields |> string_of_int) (List.length constructor_args |> string_of_int))
  in
  check_args constructor_args fields

let rec type_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (method_defns: Ast.method_defn list)
(function_defns: Ast.function_defn list) (expr: Ast.expr) context : (Typed_ast.expr, Base.Error.t) Result.t =
  let type_with_defns = type_expr struct_defns trait_defns method_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns trait_defns method_defns function_defns in
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i})

  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b})

  | Ast.Identifier id ->
    let%map (typed_id, id_type) = type_identifier struct_defns function_defns id context expr.loc in
    ({Typed_ast.loc = expr.loc; typ = id_type; node = TIdentifier typed_id})

  | Ast.Let (type_annot_maybe, var_name, let_expr)-> 
      let%bind () = is_this var_name expr.loc in
      let%bind typed_expr = type_with_defns let_expr context in
      let var_type = match type_annot_maybe with
        | Some type_annot -> if phys_equal type_annot typed_expr.typ then type_annot
          else failwith (Fmt.str "%s Type error - Let expression type %s does not match type annotation %s" 
            (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type type_annot))
        | None -> typed_expr.typ
      in
      Ok ({Typed_ast.loc = expr.loc; typ = var_type; node = TLet (type_annot_maybe, var_name, typed_expr)})

  | Ast.Constructor(var_name, struct_name, constructor_args) ->
    let%bind struct_defn = get_struct_defn struct_name struct_defns expr.loc in
    let%bind typed_constructor_args = type_constructor_args struct_defn struct_name constructor_args type_with_defns expr.loc context in
      Ok ({Typed_ast.loc = expr.loc; typ = Ast_types.TEStruct (struct_name); node = TConstructor (var_name, struct_name, typed_constructor_args)})
    
  | Ast.Assign (id, assignable_expr) -> 
      let%bind () = identifier_assignable id struct_defns context expr.loc in
      let%bind typed_expr = type_with_defns assignable_expr context in
      let%bind (typed_id, id_type) = type_identifier struct_defns function_defns id context expr.loc in 
      if phys_equal id_type typed_expr.typ then
        Ok ({Typed_ast.loc = expr.loc; typ = id_type; node = TAssign (typed_id, typed_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Trying to assign type %s to a field of type %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type id_type))

  | Ast.MethodApp (obj_name, method_name, args) ->
    let%bind struct_defn = get_obj_struct_defn obj_name struct_defns context expr.loc in
    let%bind trait_defns = get_struct_trait_defns struct_defn method_defns trait_defns expr.loc in
    let%bind method_defn_type = get_method_defn method_name trait_defns expr.loc in
    let%bind typed_args = type_args type_with_defns args context in
    Ok ({Typed_ast.loc = expr.loc; typ = method_defn_type; node = TMethodApp (obj_name, method_name, typed_args)})

  | Ast.FunctionApp(func_name, args_expr) ->
    let%bind typed_args_exprs = type_args type_with_defns args_expr context in
    let%map return_type = get_matching_function_type func_name function_defns expr.loc in
      ({Typed_ast.loc = expr.loc; typ = return_type; node = TFunctionApp (func_name, typed_args_exprs)})

  | Ast.FinishAsync (_,_,_) -> Or_error.error_string "FinishAsync Not implemented"

  | Ast.If (cond, then_expr, else_expr) ->
    let%bind typed_cond = type_with_defns cond context in
    let%bind typed_then_expr = type_block_with_defns then_expr context in
    let%bind typed_else_expr = type_block_with_defns else_expr context in
    if phys_equal typed_cond.typ TEBool then
      if phys_equal typed_then_expr typed_else_expr then
        Ok ({Typed_ast.loc = expr.loc; typ = typed_cond.typ; node = TIf (typed_cond, typed_then_expr, typed_else_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - If statement branches have different types: type1 and type2" 
          (string_of_loc expr.loc))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - If statement condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type typed_cond.typ))

  | Ast.While (cond_expr, block_expr) ->
    let%bind typed_cond_expr = type_with_defns cond_expr context in
    let%bind typed_block_expr = type_block_with_defns block_expr context in
    if phys_equal typed_cond_expr.typ TEBool then
      Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TWhile (typed_cond_expr, typed_block_expr)})
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - While loop condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type typed_cond_expr.typ))

  | Ast.For (start_expr, cond_expr, step_expr, block_expr) ->
    let%bind typed_start_expr = type_with_defns start_expr context in
    let%bind typed_cond_expr = type_with_defns cond_expr context in
    let%bind typed_step_expr = type_with_defns step_expr context in
    let%bind typed_block_expr = type_block_with_defns block_expr context in
    if phys_equal typed_cond_expr.typ TEBool then 
      if phys_equal typed_start_expr.typ TEInt && phys_equal typed_step_expr.typ TEInt then
        Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TFor (typed_start_expr, typed_cond_expr, typed_step_expr, typed_block_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - For loop start and step expressions must be integers: %s and %s" 
          (string_of_loc expr.loc) (string_of_type typed_start_expr.typ) (string_of_type typed_step_expr.typ))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - For loop condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type typed_cond_expr.typ))

  | Ast.BinOp (binop, lhs, rhs) ->
    let%bind typed_lhs = type_with_defns lhs context in
    let%bind typed_rhs = type_with_defns rhs context in
    if phys_equal typed_lhs.typ typed_rhs.typ then 
      match binop with
      | BinOpPlus | BinOpMinus | BinOpMult | BinOpIntDiv | BinOpRem ->
        if phys_equal typed_lhs.typ TEInt then
          Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Arithmetic operations can only be performed on integers: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
      | BinOpEq | BinOpNotEq | BinOpLessThan | BinOpLessThanEq | BinOpGreaterThan | BinOpGreaterThanEq ->
        if phys_equal typed_lhs.typ TEInt || phys_equal typed_lhs.typ TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Comparison operations can only be performed on integers and booleans: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
      | BinOpAnd | BinOpOr ->
        if phys_equal typed_lhs.typ TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Logical operations can only be performed on booleans: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - Binary operation operands must have the same type: %s and %s" 
        (string_of_loc expr.loc) (string_of_type typed_lhs.typ) (string_of_type typed_rhs.typ))
  (* | Ast.UnOp (unop, unop_expr) ->
    type_with_defns unop_expr context
    >>= fun typed_unop_expr ->  
    match unop with
    | UnOpNeg -> if phys_equal typed_unop_expr.typ TEInt then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TUnOp (unop, typed_unop_expr)}, TEInt)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be an integer: %s" 
          (string_of_loc expr.loc) (string_of_type typed_unop_expr.typ))
    | UnOpNot -> if phys_equal typed_unop_expr.typ TEBool then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TUnOp (unop, typed_unop_expr)}, TEBool)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be a boolean: %s" 
          (string_of_loc expr.loc) (string_of_type typed_unop_expr.typ)) *)
  | Ast.UnOp (_,_) -> Or_error.error_string "UnOp Not implemented"

and type_block_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (method_defns: Ast.method_defn list)
(function_defns: Ast.function_defn list) (Ast.Block (loc, exprs)) context =
  let type_with_defns = type_expr struct_defns trait_defns method_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns trait_defns method_defns function_defns in
  let new_context = Type_env.push_scope context in
  check_no_duplicate_var_declarations_in_block exprs loc
>>= fun () ->
  match exprs with 
  | [] -> Ok (Typed_ast.Block (loc, TEVoid, []))
  | [expr] ->
  type_with_defns expr new_context 
  >>| fun (typed_expr) -> 
    (Typed_ast.Block (loc, typed_expr.typ, [typed_expr]))
  | expr1 :: expr2 :: exprs ->
    type_with_defns expr1 new_context
    >>= fun (typed_expr1) ->
    (let updated_env =
        match typed_expr1.node with
        | TLet (_, var_name, _) -> 
        begin
          match add_variable new_context var_name typed_expr1.typ with
          | Ok updated_context -> updated_context
          | Error err -> failwith err
        end
        | TConstructor (var_name, _, _) ->
        begin
          match add_variable new_context var_name typed_expr1.typ with
          | Ok updated_context -> updated_context
          | Error err -> failwith err
        end
        | _ -> new_context in
      type_block_with_defns (Ast.Block (loc, expr2 :: exprs)) updated_env)
    >>| fun (Typed_ast.Block (_, _, typed_exprs)) -> 
      (Typed_ast.Block (loc, typed_expr1.typ, typed_expr1 :: typed_exprs))