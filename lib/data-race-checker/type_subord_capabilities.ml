open Core

open Poppy_parser.Ast_types
open Poppy_type_checker.Typed_ast
open Data_race_env

let remove_subord_capabilities env struct_name capabilities =
  List.filter
    ~f:(fun capability ->
      not (capability_fields_have_mode capability struct_name Subordinate env))
    capabilities

let remove_subord_capabilities_id env id =
  match id with
  | TVariable (var_name, var_type, caps, maybeBorrowed) -> (
    match var_type with
    | TEStruct var_struct ->
        TVariable
          ( var_name
          , var_type
          , remove_subord_capabilities env var_struct caps
          , maybeBorrowed )
    | _                      -> id (* nothing to update *) )
  | TObjField (obj_struct, obj_name, field_type, field_name, caps, maybeBorrowed) ->
      TObjField
        ( obj_struct
        , obj_name
        , field_type
        , field_name
        , remove_subord_capabilities env obj_struct caps
        , maybeBorrowed )

(* let rec remove_subord_capabilities_expr env expr =
  match expr.node with
  | TInt _ | TBoolean _ -> expr
  | TIdentifier id -> TIdentifier (remove_subord_capabilities_id env id)
  | TBlockExpr block_expr ->
    TBlockExpr (remove_subord_capabilities_block_expr env block_expr)
  | TConstructor (var_name, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (field_name, expr)) ->
            ConstructorArg
              (field_name, remove_subord_capabilities_expr env expr))
          constructor_args in
      TConstructor (var_name, class_name, updated_args) *)
