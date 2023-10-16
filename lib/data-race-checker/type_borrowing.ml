open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env


let id_maybe_borrowed = function
  | TVariable (_, _, _, maybe_borrowed) -> maybe_borrowed
  | TObjField (_, _, _, _, _, maybe_borrowed) -> maybe_borrowed

let id_is_borrowed id =
  match id_maybe_borrowed id with Some Borrowed -> true | None -> false

let type_function_reverse_borrowing env error_prefix return_type
    maybe_borrowed_ref_ret body_expr =
  match return_type with
  | TEStruct struct_name ->
      if struct_has_mode struct_name Linear env then
        match reduce_block_expr_to_obj_ids body_expr with
        | []  -> Ok ()
        | ids -> (
            if List.exists ~f:id_is_borrowed ids then
              Error
                (Error.of_string
                   (Fmt.str
                      "%s Body expression may return a borrowed type, which is not allowed.@."
                      error_prefix))
            else
              match maybe_borrowed_ref_ret with
              | Some Borrowed -> Ok () (* can reverse borrow ref*)
              | None          ->
                  Error
                    (Error.of_string
                       (Fmt.str
                          "%s Body expression may return a non-consumed id, which is not allowed as function doesn't borrow result.@."
                          error_prefix)) )
      else Ok () (* if not linear we are not worried about borrowing *)
  | _                       ->
      (* we don't check borrowing for primitive return type *)
      Ok ()