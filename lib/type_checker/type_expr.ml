open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
open Core.Result
open Core.Result.Let_syntax
open Poppy_parser.Ast

let string_of_type_list type_list =
  type_list
  |> List.map ~f:string_of_type
  |> String.concat ~sep:", "

let type_identifier id env loc =
  match id with
  | Ast.Variable var ->
    let%bind var_type = lookup_var env var loc in
    Ok (Typed_ast.TVariable (var, var_type), var_type)
  | Ast.ObjField (var_name, field_name) -> 
    let%bind var_type = lookup_var env var_name loc in
    begin
    match var_type with
    | TEStruct (struct_name) ->
      let%bind struct_defn = lookup_struct env struct_name in
      begin match struct_defn with
        | Ast.TStruct (_, _, fields) ->
          begin match List.find ~f:(fun (TField (_, _, name, _)) -> Field_name.(=) name field_name) fields with
            | Some (TField (_, field_type, _, _)) -> Ok (Typed_ast.TObjField (var_name, field_name, field_type), field_type)
            | None -> Error (Core.Error.of_string (Fmt.str "%d:%d Type error - Field %s not found in struct %s" (loc.lnum) (loc.cnum) (Field_name.to_string field_name) (Struct_name.to_string struct_name)))
          end
      end
    | _ -> Error (Core.Error.of_string (Fmt.str "%d:%d Type error - Variable %s is not a struct" (loc.lnum) (loc.cnum) (Var_name.to_string var_name)))
    end

let type_args type_expr_fn args env =
  Result.all (List.map ~f:(fun expr -> type_expr_fn expr env) args)

let type_constructor_args struct_defn struct_name constructor_args 
(type_expr_fn: Ast.expr -> env -> (Typed_ast.expr, Base.Error.t) Result.t) loc env =
match struct_defn with
| Ast.TStruct (_, _, fields) ->
  let rec check_args args field_defs =
    match args, field_defs with
    | [], [] -> Ok []
    | (Ast.ConstructorArg(field_name, expr))::arg_t, (TField (_, type_of_field, name, _))::field_t when Field_name.(=) field_name name ->
      let%bind typed_expr = type_expr_fn expr env in
      if equal_type_expr typed_expr.typ type_of_field then 
        let%bind remaining = check_args arg_t field_t in
        Ok (Typed_ast.ConstructorArg(field_name, typed_expr)::remaining)
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Constructor argument type %s does not match the expected type %s for field %s" 
          (string_of_loc loc) (string_of_type typed_expr.typ) (string_of_type type_of_field) (Field_name.to_string field_name))
    | _, _ -> Or_error.error_string (Fmt.str "%s Type error - # of constructor arguments do not match with # of struct fields for %s. Expected %s but got %s" 
          (string_of_loc loc) (Struct_name.to_string struct_name) (List.length fields |> string_of_int) (List.length constructor_args |> string_of_int))
  in
  check_args constructor_args fields

let rec type_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (impl_defns: Ast.impl_defn list)
(function_defns: Ast.function_defn list) (expr: Ast.expr) env : (Typed_ast.expr, Base.Error.t) Result.t =
  let type_with_defns = type_expr struct_defns trait_defns impl_defns function_defns in
  let type_block_with_defns = type_block_expr struct_defns trait_defns impl_defns function_defns in
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i})

  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b})

  | Ast.Identifier id ->
    let%map (typed_id, id_type) = type_identifier id env expr.loc in
    ({Typed_ast.loc = expr.loc; typ = id_type; node = TIdentifier typed_id})

  | Ast.Let (type_annot_maybe, var_name, let_expr)-> 
    let%bind () = check_variable_declarable var_name expr.loc in
    let%bind typed_expr = type_with_defns let_expr env in
    let var_type = match type_annot_maybe with
      | Some type_annot -> if equal_type_expr type_annot typed_expr.typ then type_annot
        else failwith (Fmt.str "%s Type error - Let expression type %s does not match type annotation %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type type_annot))
      | None -> typed_expr.typ
    in
    Ok ({Typed_ast.loc = expr.loc; typ = var_type; node = TLet (type_annot_maybe, var_name, typed_expr)})

  | Ast.Constructor(var_name, struct_name, constructor_args) ->
    let%bind struct_defn = lookup_struct env struct_name in
    let%bind typed_constructor_args = type_constructor_args struct_defn struct_name constructor_args type_with_defns expr.loc env in
      Ok ({Typed_ast.loc = expr.loc; typ = TEStruct (struct_name); node = TConstructor (var_name, struct_name, typed_constructor_args)})
    
  | Ast.Assign (id, assignable_expr) -> 
      let%bind () = check_identifier_assignable id env expr.loc in
      let%bind typed_expr = type_with_defns assignable_expr env in
      let%bind (typed_id, id_type) = type_identifier id env expr.loc in 
      if equal_type_expr id_type typed_expr.typ then
        Ok ({Typed_ast.loc = expr.loc; typ = id_type; node = TAssign (typed_id, typed_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Trying to assign type %s to a field of type %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type id_type))

  | Ast.MethodApp(receiver_var, method_name, args_expr) ->
    let%bind receiver_type = lookup_var env receiver_var expr.loc in
    begin match receiver_type with
    | TEStruct (receiver_struct_name) ->
      (* let%bind _implemented_traits = lookup_impl env receiver_struct_name in *)
      let%bind method_defn = lookup_method_in_impl env receiver_struct_name method_name in
      begin match method_defn with
      | Ast.TMethod (method_signature, _) ->
        let param_types = List.map ~f:(function Param (param_type, _, _, _) -> param_type) method_signature.params in
        let%bind typed_args = type_args type_with_defns args_expr env in
        if not (equal_type_expr_list param_types (List.map typed_args ~f:(fun arg -> arg.typ))) then
          Error (Core.Error.of_string (Fmt.str "%s Type error - Method %s expected arguments of type %s but got %s" 
            (string_of_loc expr.loc) (Method_name.to_string method_name) (string_of_type_list param_types) (string_of_type_list (List.map typed_args ~f:(fun arg -> arg.typ)))))
        else
          Ok ({Typed_ast.loc = expr.loc; typ = method_signature.return_type; node = TMethodApp (receiver_var, method_name, typed_args)})
      end
    | _ -> Error (Core.Error.of_string (Fmt.str "%s Type error - Method %s can only be called on objects of type struct, but receiver is of type %s" 
          (string_of_loc expr.loc) (Method_name.to_string method_name) (string_of_type receiver_type)))
    end          

  | Ast.FunctionApp(func_name, args_expr) ->
    let%bind typed_args_exprs = type_args type_with_defns args_expr env in
    let%bind func_defn = lookup_function env func_name in 
    begin match func_defn with
    | Ast.TFunction (function_signature, _) ->
      let param_types = List.map function_signature.params ~f:(fun (Param (param_type, _, _, _)) -> param_type) in
      let arg_types = List.map typed_args_exprs ~f:(fun arg -> arg.typ) in
      if equal_type_expr_list param_types arg_types then
        Ok ({Typed_ast.loc = expr.loc; typ = function_signature.return_type; node = TFunctionApp (func_name, typed_args_exprs)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Function %s expected arguments of type %s but got %s" 
          (string_of_loc expr.loc) (Function_name.to_string func_name) (List.map param_types ~f:string_of_type |> String.concat ~sep:", ") (List.map arg_types ~f:string_of_type |> String.concat ~sep:", "))
    end

  | FinishAsync (async_exprs, curr_thread_expr) ->
      (* Check async expressions type-check - note they have access to same env, as not
          being checked for data races in this stage of type-checking *)
      Result.all
        (List.map
            ~f:(fun (AsyncExpr async_block_expr) ->
              type_block_with_defns async_block_expr env
              >>| fun (typed_async_block_expr, _) ->
              Typed_ast.AsyncExpr typed_async_block_expr)  
            async_exprs)
      >>= fun typed_async_exprs ->
      type_block_with_defns curr_thread_expr env
      >>| fun (typed_curr_thread_expr, curr_thread_expr_type) ->
      { Typed_ast.loc = expr.loc;
        typ = curr_thread_expr_type;
        node = TFinishAsync (typed_async_exprs, typed_curr_thread_expr);
          }
    
  
  | Printf (format_str, args) ->
    let%bind typed_args = type_args type_with_defns args env in
    Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TPrintf (format_str, typed_args)})

  | Ast.If (cond, then_expr, else_expr) ->
    let%bind typed_cond = type_with_defns cond env in
    let%bind (typed_then_block_expr, typed_then_type) = type_block_with_defns then_expr env in
    let%bind (typed_else_block_expr, typed_else_type) = type_block_with_defns else_expr env in
    if equal_type_expr typed_cond.typ TEBool then
      if phys_equal typed_then_type typed_else_type then
        Ok ({Typed_ast.loc = expr.loc; typ = typed_cond.typ; node = TIf (typed_cond, typed_then_block_expr, typed_else_block_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - If statement branches have different types: %s and %s" 
          (string_of_loc expr.loc) (string_of_type typed_then_type) (string_of_type typed_else_type))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - If statement condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type typed_cond.typ))
  
  | Ast.While (cond_expr, block_expr) ->
    let%bind typed_cond_expr = type_with_defns cond_expr env in
    let%bind (typed_block_expr, _) = type_block_with_defns block_expr env in
    if equal_type_expr typed_cond_expr.typ TEBool then
      Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TWhile (typed_cond_expr, typed_block_expr)})
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - While loop condition is not a boolean: %s" 
        (string_of_loc expr.loc) (string_of_type typed_cond_expr.typ))

| Ast.For (start_expr, cond_expr, step_expr, Ast.Block(block_loc, block_expr)) ->
    let desugared_while_expr_node = 
        Ast.While (cond_expr, Ast.Block(block_loc, block_expr @ [step_expr])) 
    in
    let desugared_while_expr = { loc = expr.loc; node = desugared_while_expr_node } in
    let%bind (typed_block_expr, _) = type_block_with_defns 
        (Ast.Block(expr.loc, [start_expr; desugared_while_expr])) env 
    in
    Ok ({Typed_ast.loc = expr.loc; typ = TEVoid; node = TBlockExpr (typed_block_expr)})
      
  
  | Ast.BinOp (binop, lhs, rhs) ->
    let%bind typed_lhs = type_with_defns lhs env in
    let%bind typed_rhs = type_with_defns rhs env in
    if equal_type_expr typed_lhs.typ typed_rhs.typ then 
      match binop with
      | BinOpPlus | BinOpMinus | BinOpMult | BinOpIntDiv | BinOpRem ->
        if equal_type_expr typed_lhs.typ TEInt then
          Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Arithmetic operations can only be performed on integers: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
      | BinOpEq | BinOpNotEq | BinOpLessThan | BinOpLessThanEq | BinOpGreaterThan | BinOpGreaterThanEq ->
        if equal_type_expr typed_lhs.typ TEInt || equal_type_expr typed_lhs.typ TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Comparison operations can only be performed on integers and booleans: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
      | BinOpAnd | BinOpOr ->
        if equal_type_expr typed_lhs.typ TEBool then
          Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBinOp (binop, typed_lhs, typed_rhs)})
        else
          Or_error.error_string 
          (Fmt.str "%s Type error - Logical operations can only be performed on booleans: %s" 
            (string_of_loc expr.loc) (string_of_type typed_lhs.typ))
    else
      Or_error.error_string 
      (Fmt.str "%s Type error - Binary operation operands must have the same type: %s and %s" 
        (string_of_loc expr.loc) (string_of_type typed_lhs.typ) (string_of_type typed_rhs.typ))

  | Ast.UnOp (unop, unop_expr) ->
    let%bind typed_unop_expr = type_with_defns unop_expr env in
    match unop with
    | UnOpNeg -> if equal_type_expr typed_unop_expr.typ TEInt then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TUnOp (unop, typed_unop_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be an integer: %s" 
          (string_of_loc expr.loc) (string_of_type typed_unop_expr.typ))
    | UnOpNot -> if equal_type_expr typed_unop_expr.typ TEBool then 
      Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TUnOp (unop, typed_unop_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Unary operation operand must be a boolean: %s" 
          (string_of_loc expr.loc) (string_of_type typed_unop_expr.typ));


and type_block_expr struct_defns trait_defns impl_defns function_defns (Ast.Block (loc, exprs)) env =
let type_with_defns = type_expr struct_defns trait_defns impl_defns function_defns in
let type_block_with_defns = type_block_expr struct_defns trait_defns impl_defns function_defns in
let%bind () = check_no_duplicate_var_declarations_in_block exprs loc in
  match exprs with 
  | [] -> Ok (Typed_ast.Block (loc, TEVoid, []), TEVoid)
  | [expr] ->
    let%map typed_expr = type_with_defns expr env in
    (Typed_ast.Block (loc, typed_expr.typ, [typed_expr]), typed_expr.typ)
  | expr1 :: expr2 :: exprs ->
      let%bind typed_expr1 = type_with_defns expr1 env in
  (let updated_env =
      match typed_expr1.node with
      | TLet (_, var_name, _) -> (add_var_to_block_scope env var_name typed_expr1.typ)
      | TConstructor (var_name, _, _) -> (add_var_to_block_scope env var_name typed_expr1.typ)
      | _ -> env in
      type_block_with_defns (Ast.Block (loc, expr2 :: exprs)) updated_env)
  >>| fun (typed_block, block_type) -> 
    match typed_block with
    | Typed_ast.Block (_, _, typed_exprs) -> 
        (Typed_ast.Block (loc, typed_expr1.typ, typed_expr1 :: typed_exprs), block_type)
