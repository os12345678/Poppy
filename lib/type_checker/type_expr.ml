open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
open Core.Result
open Core.Result.Let_syntax
open Poppy_parser.Ast
open Free_obj_vars

let dedup_free_vars free_vars =
  List.dedup_and_sort
    ~compare:(fun (var_name_1, _, _) (var_name_2, _, _) ->
      if Var_name.(=) var_name_1 var_name_2 then 0 else 1)
    free_vars

let string_of_type_list type_list =
  type_list
  |> List.map ~f:string_of_type
  |> String.concat ~sep:", "

  let type_identifier id (struct_defns: struct_defn list) borrowed_vars env loc =
    match id with
    | Ast.Variable var ->
      let%bind var_type = lookup_var env var loc in
      let capabilities = 
      match var_type with 
      | TEStruct sname -> get_struct_capabilities2 sname struct_defns 
      | _ -> [] in
      Printf.printf "var: %s, var_type: %s, capabilities: %s\n" 
      (Var_name.to_string var) 
      (string_of_type var_type) 
      (String.concat ~sep:", " (List.map ~f:(fun caps -> string_of_cap caps) capabilities));
          let isBorrowed =
        if elem_in_list var borrowed_vars then Some Borrowed else None in
      Ok (Typed_ast.TVariable (var, var_type, capabilities, isBorrowed), var_type)
  
      | Ast.ObjField (var_name, field_name) -> 
        let%bind obj_struct = get_obj_struct_defn var_name env loc in
        begin match obj_struct with 
        | TStruct (sname, _, fields) -> 
         let isBorrowed =
          if elem_in_list var_name borrowed_vars then Some Borrowed else None in
         begin match List.find ~f:(fun (TField (_, _, name, _)) -> Field_name.(=) name field_name) fields with
          | Some (TField (_, field_type, _, _)) -> 
            let field_capabilities = get_method_field_capabilities2 sname struct_defns in
              Ok (Typed_ast.TObjField (sname, var_name, field_type, field_name, field_capabilities, isBorrowed), field_type)
              | None -> Error (Core.Error.of_string (Fmt.str "%d:%d Type error - Field %s not found in struct %s" (loc.lnum) (loc.cnum) (Field_name.to_string field_name) (Struct_name.to_string sname)))
            end
        end

let type_args type_expr_fn args env =
  Result.all (List.map ~f:(fun expr -> type_expr_fn expr env) args)

let type_constructor_args struct_defns struct_name constructor_args 
(type_expr_fn: Ast.expr -> env -> (Typed_ast.expr, Base.Error.t) Result.t) loc env =
  let open Result in
  let fields = Type_env.get_struct_fields2 struct_name struct_defns in
  Result.all (List.map ~f:(fun (Ast.ConstructorArg (field_name, expr)) ->
    match List.find ~f:(fun (TField (_, _, name, _)) -> Field_name.(=) name field_name) fields with
    | None -> Error (Core.Error.of_string (Fmt.str "%s Type error - Field %s not found in struct %s" 
        (string_of_loc loc) (Field_name.to_string field_name) (Struct_name.to_string struct_name)))
    | Some (TField (_, field_type, _, _)) ->
      type_expr_fn expr env >>= fun typed_expr ->
      if equal_type_expr field_type typed_expr.typ then
        Ok (Typed_ast.ConstructorArg (field_name, typed_expr))
      else
        Error (Core.Error.of_string (Fmt.str "%s Type error - Type mismatch for field %s in struct %s, expected %s, got %s" 
          (string_of_loc loc) (Field_name.to_string field_name) (Struct_name.to_string struct_name) 
          (string_of_type field_type) (string_of_type typed_expr.typ)))
  ) constructor_args)
  
let rec type_expr (struct_defns: Ast.struct_defn list) (trait_defns: Ast.trait_defn list) (impl_defns: Ast.impl_defn list)
(function_defns: Ast.function_defn list) (borrowed_vars: Var_name.t list) (expr: Ast.expr) env : (Typed_ast.expr, Base.Error.t) Result.t =
let type_with_defns = type_expr struct_defns trait_defns impl_defns function_defns borrowed_vars in
let type_block_with_defns = type_block_expr struct_defns trait_defns impl_defns function_defns borrowed_vars in 
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i})

  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b})

  | Ast.Identifier id ->
    let%map (typed_id, id_type) = type_identifier id struct_defns borrowed_vars env expr.loc in
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

  | Ast.Constructor (struct_name, constructor_args) ->
    type_constructor_args struct_defns struct_name constructor_args 
      type_with_defns expr.loc env
    >>| fun typed_constructor_args ->
      ({Typed_ast.loc = expr.loc; typ = TEStruct (struct_name); node = TConstructor (struct_name, typed_constructor_args)})
      
  | Ast.Assign (id, assignable_expr) -> 
      let%bind () = check_identifier_assignable id env expr.loc in
      let%bind typed_expr = type_with_defns assignable_expr env in
      let%bind (typed_id, id_type) = type_identifier id struct_defns borrowed_vars env expr.loc in 
      if equal_type_expr id_type typed_expr.typ then
        Ok ({Typed_ast.loc = expr.loc; typ = id_type; node = TAssign (typed_id, typed_expr)})
      else
        Or_error.error_string 
        (Fmt.str "%s Type error - Trying to assign type %s to a field of type %s" 
          (string_of_loc expr.loc) (string_of_type typed_expr.typ) (string_of_type id_type))
  | Ast.Consume (id) ->
    let%bind () = check_identifier_consumable id env expr.loc in
    let%map (typed_id, id_type) = type_identifier id struct_defns borrowed_vars env expr.loc in 
    ({Typed_ast.loc = expr.loc; typ = id_type; node = TConsume(typed_id)})

  | Ast.MethodApp(receiver_var, method_name, args_expr) ->
    let%bind receiver_type = lookup_var env receiver_var expr.loc in
    begin match receiver_type with
    | TEStruct (receiver_struct_name) ->
        let%bind trait_names = lookup_impl env receiver_struct_name in
        let find_trait_and_method_signature trait_name =
            match lookup_method_in_impl env receiver_struct_name method_name with
            | Ok (Ast.TMethod (method_signature, _)) -> Some (trait_name, method_signature)
            | _ -> None
        in
        let trait_and_method_signature = List.find_map ~f:find_trait_and_method_signature trait_names in
        begin match trait_and_method_signature with
        | Some (trait_name, method_signature) ->
            let caps = get_struct_capabilities2 receiver_struct_name struct_defns in
            let param_types = List.map ~f:(function Param (param_type, _, _, _) -> param_type) method_signature.params in
            let%bind typed_args = type_args type_with_defns args_expr env in
            if not (equal_type_expr_list param_types (List.map typed_args ~f:(fun arg -> arg.typ))) then
                Error (Core.Error.of_string (Fmt.str "%s Type error - Method %s expected arguments of type %s but got %s" 
                    (string_of_loc expr.loc) (Method_name.to_string method_name) (string_of_type_list param_types) (string_of_type_list (List.map typed_args ~f:(fun arg -> arg.typ)))))
            else
                Ok ({Typed_ast.loc = expr.loc; typ = method_signature.return_type; node = TMethodApp (receiver_var, receiver_struct_name, trait_name, method_name, caps, typed_args)})
        | None -> 
            Error (Core.Error.of_string (Fmt.str "%s Type error - Method %s not found in any implemented trait for struct %s" 
                (string_of_loc expr.loc) (Method_name.to_string method_name) (Struct_name.to_string receiver_struct_name)))
        end
    | _ -> 
        Error (Core.Error.of_string (Fmt.str "%s Type error - Method %s can only be called on objects of type struct, but receiver is of type %s" 
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
    (* Type check async expressions *)
    Result.all (List.map ~f:(type_async_expr type_block_with_defns env struct_defns) async_exprs)
    >>= fun typed_async_exprs ->
    (* Type check current thread expression *)
    type_block_with_defns curr_thread_expr env
    >>= fun (typed_curr_thread_expr, curr_thread_expr_type) ->
    (* Collect free variables and types from both expressions *)
    let free_vars_and_types =
      List.concat_map
        ~f:(fun (Typed_ast.AsyncExpr(free_vars, _)) -> free_vars)
        typed_async_exprs @
      dedup_free_vars (free_obj_vars_block_expr env struct_defns typed_curr_thread_expr)
    in
    (* Construct typed AST *)
    let typed_ast = {
      Typed_ast.loc = expr.loc;
      typ = curr_thread_expr_type;
      node = TFinishAsync
        ( typed_async_exprs
        , free_vars_and_types
        , typed_curr_thread_expr
        )
    } in
    Ok typed_ast
      
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
    begin match unop with
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
          (string_of_loc expr.loc) (string_of_type typed_unop_expr.typ))
      end

  and type_async_expr  (type_block_with_defns: block_expr -> env -> (T.block_expr * type_expr, Core.Error.t) t) 
  env struct_defns (Ast.AsyncExpr async_block_expr) =
    let%bind typed_block_with_defns, _ = type_block_with_defns async_block_expr env in
    let free_obj_vars_expr =
      dedup_free_vars (free_obj_vars_block_expr env struct_defns typed_block_with_defns) in
    type_block_with_defns async_block_expr env
    |> function
    | Ok (desugared_async_block_expr, _) ->
        Ok (Typed_ast.AsyncExpr (free_obj_vars_expr, desugared_async_block_expr))
    | Error err -> Error err
  


and type_block_expr struct_defns trait_defns impl_defns function_defns (borrowed_vars: Var_name.t list) (Ast.Block (loc, exprs)) env : (Typed_ast.block_expr * type_expr, Core.Error.t) t=
let type_with_defns = type_expr struct_defns trait_defns impl_defns function_defns borrowed_vars in
let type_block_with_defns = type_block_expr struct_defns trait_defns impl_defns function_defns borrowed_vars in
let%bind () = check_no_duplicate_var_declarations_in_block exprs loc in
match exprs with 
| [] -> Ok (Typed_ast.Block (loc, TEVoid, []), TEVoid)
| [expr] ->
    let%map typed_expr = type_with_defns expr env in
    (Typed_ast.Block (loc, typed_expr.typ, [typed_expr]), typed_expr.typ)
| expr1 :: expr2 :: exprs ->
    let%bind typed_expr1 = type_with_defns expr1 env in
    let updated_env =
        match typed_expr1.node with
        | TLet (_, var_name, _) -> (add_var_to_block_scope env var_name typed_expr1.typ)
        (* | TConstructor (var_name, _, _) -> (add_var_to_block_scope env var_name typed_expr1.typ) *)
        | _ -> env in
    type_block_with_defns (Ast.Block (loc, expr2 :: exprs)) updated_env
>>| fun (typed_block, block_type) -> 
    match typed_block with
    | Typed_ast.Block (_, _, typed_exprs) -> 
        (Typed_ast.Block (loc, typed_expr1.typ, typed_expr1 :: typed_exprs), block_type)