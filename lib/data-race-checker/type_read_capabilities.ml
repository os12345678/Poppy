module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types
module E = Data_race_env

let remove_read_capabilities id =
  let filtered_capabilities =
    List.filter
      (fun (A.TCapability (mode, _)) -> not (mode = Read))
      (E.get_identifier_capabilities id) in
  E.set_identifier_capabilities id filtered_capabilities


let rec type_read_capabilities_expr (expr: T.expr) : T.expr =
  match expr.node with
  | T.TInt _ | T.TBoolean _ -> expr
  | T.TIdentifier id -> { expr with node = T.TIdentifier id }
  | T.TLet (type_expr, name, bound_expr) ->
    let updated_rhs = type_read_capabilities_expr bound_expr in
    { expr with node = T.TLet (type_expr, name, updated_rhs) }
  | T.TBlockExpr block_expr ->
    {expr with node = T.TBlockExpr (type_read_capabilities_block_expr block_expr)}
  | T.TAssign (id, assigned_expr) -> {expr with node = 
    T.TAssign
      (remove_read_capabilities id
      , type_read_capabilities_expr assigned_expr )}
  | T.TMethodApp (var_name, method_name, args) ->
    {expr with node = T.TMethodApp
      (var_name
      , method_name
      , List.map type_read_capabilities_expr args )}
  | T.TFunctionApp (func_name, args) ->
    {expr with node = 
      T.TFunctionApp
        (func_name, List.map type_read_capabilities_expr args)}
  | TConstructor (var_name, struct_name, constructor_args) ->
    let updated_args = 
      List.map 
        (fun (T.ConstructorArg (field_name, expr)) ->
          T.ConstructorArg (field_name, type_read_capabilities_expr expr))
        constructor_args in
    {expr with node = T.TConstructor (var_name, struct_name, updated_args)}
  | T.TPrintf (format_str, args) -> {expr with node = 
    T.TPrintf (format_str, List.map type_read_capabilities_expr args)}
    (* TODO: START REVIEW  *)
  | T.TFinishAsync (async_exprs, curr_thread_expr) -> {expr with node = 
    T.TFinishAsync
      ( List.map
          (fun (T.AsyncExpr (expr)) ->
            T.AsyncExpr (type_read_capabilities_block_expr expr))
          async_exprs
      , type_read_capabilities_block_expr curr_thread_expr )}
    (* TODO: END REVIEW *)
    | T.TIf (cond_expr, then_expr, else_expr) -> {expr with node = 
      T.TIf
        ( type_read_capabilities_expr cond_expr
        , type_read_capabilities_block_expr then_expr
        , type_read_capabilities_block_expr else_expr )}
  | T.TWhile (cond_expr, loop_expr) -> {expr with node = 
      T.TWhile
        ( type_read_capabilities_expr cond_expr
        , type_read_capabilities_block_expr loop_expr )}
  | T.TBinOp (binop, expr1, expr2) -> {expr with node = 
      T.TBinOp
        ( binop
        , type_read_capabilities_expr expr1
        , type_read_capabilities_expr expr2 )}
  | T.TUnOp (unop, expr) -> {expr with node = 
      T.TUnOp (unop, type_read_capabilities_expr expr)}

and type_read_capabilities_block_expr (T.Block (loc, type_block_expr, exprs)) =
  let updated_exprs = List.map type_read_capabilities_expr exprs in
  T.Block (loc, type_block_expr, updated_exprs)