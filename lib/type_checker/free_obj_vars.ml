open Poppy_parser.Ast_types
(* open Poppy_parser *)
open Type_env
open Core  
(* open Core.Resultcd  *)
(* open Core.Result.Let_syntax *)

module A = Poppy_parser.Ast
module T = Typed_ast
module E = Poppy_parser.Ast_types

let remove_bound_var bound_var_name free_vars_list =
  List.filter ~f:(fun (var_name, _, _) -> not (Var_name.(=) var_name bound_var_name)) free_vars_list

let union_free_vars_lists free_vars_lists =
  (* List.dedup_and_sort
    ~compare:(fun name_1 name_2 -> if name_1 = name_2 then 0 else 1) *)
    List.concat free_vars_lists

let free_obj_vars_identifier id env = 
  match id with 
  | T.TVariable (var_name, var_type, _, _) -> (
    match var_type with
    | TEStruct struct_name ->
        [(var_name, struct_name, get_struct_capabilities struct_name env)]
    | _                       -> [] )
  | TObjField (obj_struct, obj_name, _, _, _, _) ->
      [(obj_name, obj_struct, get_method_field_capabilities obj_struct env)]
    

let rec free_obj_vars_expr (env: env) (expr: T.expr) =
  match expr.node with 
  | T.TInt _ -> []
  | TBoolean _ -> []
  | TIdentifier id -> free_obj_vars_identifier id env
  | TBlockExpr block_expr -> free_obj_vars_block_expr env block_expr
  | TConstructor (_, _, constructor_args) ->
    union_free_vars_lists
    (List.map
       ~f:(fun (ConstructorArg (_, expr)) -> free_obj_vars_expr env expr)
       constructor_args)
  | TLet (_, _, bound_expr) -> free_obj_vars_expr env bound_expr
  | TAssign (identifier, assigned_expr) ->
      free_obj_vars_expr env assigned_expr
      |> fun free_vars_assigned_expr ->
      free_obj_vars_identifier identifier env @ free_vars_assigned_expr
  | TConsume id -> free_obj_vars_identifier id env
  | TMethodApp (obj_name, obj_struct, _, _, args_exprs) ->
      (obj_name, obj_struct, get_struct_capabilities obj_struct env)
      :: union_free_vars_lists (List.map ~f:(free_obj_vars_expr env) args_exprs)
  | TFunctionApp (_, args_exprs) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr env) args_exprs)
  | TPrintf (_, args_exprs) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr env) args_exprs)
  | TFinishAsync (async_exprs, _, curr_thread_expr) ->
    let free_vars_async_exprs =
      List.map
        ~f:(fun (AsyncExpr (_, block_expr)) ->
          free_obj_vars_block_expr env block_expr)
        async_exprs in
    union_free_vars_lists
      (free_obj_vars_block_expr env curr_thread_expr :: free_vars_async_exprs)
  | TIf (cond_expr, then_expr, else_expr) ->
    union_free_vars_lists
      [ free_obj_vars_expr env cond_expr
      ; free_obj_vars_block_expr env then_expr
      ; free_obj_vars_block_expr env else_expr ]
  | TWhile (cond_expr, loop_expr) ->
      union_free_vars_lists
        [ free_obj_vars_expr env cond_expr
        ; free_obj_vars_block_expr env loop_expr ]
  | TBinOp (_, expr1, expr2) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr env) [expr1; expr2])
  | TUnOp (_, expr) -> free_obj_vars_expr env expr


  and free_obj_vars_block_expr env (T.Block (loc, block_type, block_exprs)) =
  match block_exprs with
  | []            -> []
  | expr :: exprs -> (
      free_obj_vars_block_expr env (Block (loc, block_type, exprs))
      |> fun exprs_free_vars ->
      match expr.node with
      (* If let binding then need to remove bound variable from block's free vars *)
      | TLet (_, var_name, bound_expr) ->
          free_obj_vars_expr env bound_expr
          |> fun bound_expr_free_vars ->
          union_free_vars_lists
            [bound_expr_free_vars; remove_bound_var var_name exprs_free_vars]
      | _ ->
          free_obj_vars_expr env expr
          |> fun expr_free_vars -> union_free_vars_lists [expr_free_vars; exprs_free_vars]
      )