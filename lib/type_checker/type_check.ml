open Core
open Poppy_parser.Ast

(* Define Typing Context *)
type context = {
  variables: (string, typ) Map.Poly.t;
  functions: (string, (typ list * typ)) Map.Poly.t;
}
let empty_context = {
  variables = Map.Poly.empty;
  functions = Map.Poly.empty;
}
let initial_context = (* Initialise context with external functions i.e print*)
  let functions = empty_context.functions in
  let functions = Map.set functions ~key:"print" ~data:([String; Int], Void) in
  { empty_context with functions }

(* Expression Typing *)
let rec check_expr (ctx : context) (expr : expr) : typ =
  match expr with
  | IntLiteral _ -> Int
  | BoolLiteral _ -> Bool
  | StringType _ -> String
  | VoidType -> Void
  | Id id ->
    (match Map.find ctx.variables id with
     | Some typ -> typ
     | None -> raise (Failure (Printf.sprintf "Unbound variable: %s" id)))
  | BinOp (op, lhs, rhs) ->
    let lhs_typ = check_expr ctx lhs in
    let rhs_typ = check_expr ctx rhs in
    (match op with
     | Plus | Minus | Times | Div -> (* Assuming only Int operations *)
       if equal_typ lhs_typ Int && equal_typ rhs_typ Int then Int
       else raise (Failure "Type error: expected int for arithmetic operation")
     | Lt | Gt | Leq | Geq | Eq | Neq -> (* Assuming Int and Bool comparisons *)
       if equal_typ lhs_typ rhs_typ && (equal_typ lhs_typ Int || equal_typ lhs_typ Bool) then Bool
       else raise (Failure "Type error: expected int or bool for comparison operation")
     | And | Or | Xor -> (* Assuming only Bool operations *)
       if equal_typ lhs_typ Bool && equal_typ rhs_typ Bool then Bool
       else raise (Failure "Type error: expected bool for logical operation"))
  | Not expr ->
    let expr_typ = check_expr ctx expr in
    if equal_typ expr_typ Bool then Bool
    else raise (Failure "Type error: expected bool for logical operation")
  | Unit -> Void
  | StringLiteral _ -> String
  | Lambda (params, body) ->
    let ctx_with_params = 
      List.fold params ~init:ctx ~f:(fun acc_ctx (Param (Id id, Type typ)) ->
        { acc_ctx with variables = Map.set acc_ctx.variables ~key:id ~data:typ }) in
    let body_typ = check_expr ctx_with_params body in
    Function (List.map params ~f:(fun (Param (_, Type typ)) -> typ), body_typ)
    | Call (id, args) ->
      if String.equal id "print" then (* handle print arguments seperately *)
        (match args with
         | [arg1; arg2] ->
           let arg1_typ = check_expr ctx arg1 in
           if not (equal_typ arg1_typ String) then
             raise (Failure (Printf.sprintf "Type error: expected String for the first argument of print but got %s" (Sexp.to_string_hum (sexp_of_typ arg1_typ))));
           let arg2_typ = check_expr ctx arg2 in
           (match arg2_typ with
            | Int | Bool -> ()
            | _ -> raise (Failure (Printf.sprintf "Type error: invalid type for the second argument of print: %s. Expected Int or Bool." (Sexp.to_string_hum (sexp_of_typ arg2_typ)))));
           Void
         | _ -> raise (Failure "Type error: print function expects exactly 2 arguments"))
      else
    (match Map.find ctx.functions id with
     | Some (param_typs, ret_typ) ->
       if List.length param_typs <> List.length args then
         raise (Failure (Printf.sprintf "Type error: expected %d arguments but got %d" (List.length param_typs) (List.length args)));
       List.iter2_exn param_typs args ~f:(fun param_typ arg ->
         let arg_typ = check_expr ctx arg in
         if not (equal_typ param_typ arg_typ) then
           raise (Failure (Printf.sprintf "Type error: expected %s but got %s" (Sexp.to_string_hum (sexp_of_typ param_typ)) (Sexp.to_string_hum (sexp_of_typ arg_typ)))));
       ret_typ
     | None -> raise (Failure (Printf.sprintf "Unbound function: %s" id)))
  | unimplemented_expression ->
    let sexp = sexp_of_expr unimplemented_expression in
    raise (Failure (Printf.sprintf "Type checking not implemented for this expression: %s" (Sexp.to_string_hum sexp)))

(* Statement Typing *)
let rec check_statement (ctx : context) (stmt : statement) : context =
  match stmt with
  | Let ((Id id, Type typ), expr) ->
    let expr_typ = check_expr ctx expr in
    if equal_typ typ expr_typ then
      { ctx with variables = Map.set ctx.variables ~key:id ~data:typ }
    else
      raise (Failure (Printf.sprintf "Type error: expected %s but got %s" (Sexp.to_string_hum (sexp_of_typ typ)) (Sexp.to_string_hum (sexp_of_typ expr_typ))))
  | Assign (id, expr) ->
    let expected_typ = match Map.find ctx.variables id with
      | Some typ -> typ
      | None -> raise (Failure (Printf.sprintf "Unbound variable: %s" id)) in
    let expr_typ = check_expr ctx expr in
    if equal_typ expected_typ expr_typ then ctx
    else raise (Failure (Printf.sprintf "Type error: expected %s but got %s" (Sexp.to_string_hum (sexp_of_typ expected_typ)) (Sexp.to_string_hum (sexp_of_typ expr_typ))))
  | If (cond, then_stmt, else_stmt) ->
    let cond_typ = check_expr ctx cond in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: expected bool in condition expression");
    let ctx1 = check_statement ctx then_stmt in
    let ctx2 = check_statement ctx else_stmt in
    let merged_variables = Map.merge_skewed ctx1.variables ctx2.variables ~combine:(fun ~key:_ v1 _ -> v1) in
    { ctx with variables = merged_variables }
  | While (cond, body) ->
    let cond_typ = check_expr ctx cond in
    if not (equal_typ cond_typ Bool) then
      raise (Failure "Type error: expected bool in condition expression");
    let _ = check_statement ctx body in
    ctx
  | Block stmts ->
    List.fold stmts ~init:ctx ~f:check_statement
  | FuncDecl (Id id, params, Type ret_typ, body) -> (* if main function, dont allow return statement*)
    let ctx' = List.fold params ~init:ctx ~f:(fun ctx (Param (Id id, Type typ)) ->
      { ctx with variables = Map.set ctx.variables ~key:id ~data:typ }) in
    let _ = List.fold body ~init:ctx' ~f:check_statement in
    { ctx with functions = Map.set ctx.functions ~key:id ~data:(List.map params ~f:(fun (Param (_, Type typ)) -> typ), ret_typ) }
  | Return expr ->
    let _ = check_expr ctx expr in
    ctx
  | Expr expr ->
    let _ = check_expr ctx expr in
    ctx
  | unimplement_statement ->
    let sexp = sexp_of_statement unimplement_statement in
    raise (Failure (Printf.sprintf "Type checking not implemented for this statement: %s" (Sexp.to_string_hum sexp)))

(* Traverse the Ast *)
let check_program (ast : statement list) : unit =
  let _ = List.fold ast ~init:initial_context ~f:check_statement in
  ()
