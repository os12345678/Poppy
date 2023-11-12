open Core

module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types
module E = Data_race_env
module U = Update_identifier_capabilities
module L = Type_alias_liveliness

let type_linear_obj_method_args env obj_name obj_struct args_ids loc =
  if E.struct_has_mode obj_struct Linear env then
    if List.exists ~f:(E.identifier_matches_var_name obj_name) args_ids then
      Error
        (Error.of_string
           (Fmt.str "%s One of linear object %s's method's arguments aliases it@."
              (A.string_of_loc loc) (A.Var_name.to_string obj_name)))
    else Ok () (* no aliasing in arguments *)
  else (* not linear so we don't care *) Ok ()

  let type_linear_args env args_ids loc =
    let linear_args_ids =
      List.filter ~f:(fun arg_id -> E.identifier_has_mode arg_id Linear env) args_ids
    in
    let matching_ids = function
      | T.TVariable (var_name, _, _, _) ->
          List.filter ~f:(E.identifier_matches_var_name var_name) args_ids
      | T.TObjField _ as id             -> List.filter ~f:(fun arg_id -> phys_equal id arg_id) args_ids
    in
    (* for all linear identifiers, make sure no other identifier matches that linear
       identifier *)
    if
      List.for_all
        ~f:(fun linear_arg_id -> List.length (matching_ids linear_arg_id) = 1)
        linear_args_ids
    then Ok ()
    else
      Error
        (Error.of_string
           (Fmt.str "%s Linear arguments are duplicated@." (A.string_of_loc loc)))
  

let type_linear_object_references obj_name obj_struct struct_defns block_expr =
  let filter_linear_caps_fn _ curr_capability =
    not (E.capability_fields_have_mode curr_capability obj_struct A.Linear struct_defns) in
  let obj_aliases =
    E.find_aliases_in_block_expr ~should_match_fields:false obj_name block_expr in
  let aliases_to_remove_linearity =
    E.find_aliases_in_block_expr ~should_match_fields:true obj_name block_expr
    (* match fields since if x is not linear, x.f isn't *) in
  U.update_matching_identifier_caps_block_expr aliases_to_remove_linearity
    filter_linear_caps_fn block_expr
  |> fun updated_block_expr ->
  L.type_alias_liveness_block_expr obj_name obj_aliases filter_linear_caps_fn []
    updated_block_expr
  |> fun (typed_linear_obj_ref_block_expr, _) -> typed_linear_obj_ref_block_expr


(* We don't allow linear objects to be aliased by an assignment. *)
let type_linear_assign_expr env = function
  | T.TAssign (_, assigned_expr) ->
      if E.type_has_mode assigned_expr.typ Linear env then
        if List.is_empty (E.reduce_expr_to_obj_ids assigned_expr) then Ok ()
        else
          Error
            (Error.of_string
               (Fmt.str
                  "%s Error: Can only assign a linear variable if it has been consumed@."
                  (A.string_of_loc assigned_expr.loc)))
      else Ok ()
  | _ -> Ok ()

let rec type_linear_capabilities_block_expr env (T.Block (loc, type_expr, exprs)) =
  match exprs with
  | []                  -> Ok (T.Block (loc, type_expr, exprs))
  | expr :: other_exprs ->
      let open Result in
      type_linear_assign_expr env expr.node
      >>= fun () ->
      (* if this expression is a declaration of a linear object, we type it in subsequent
         exprs + possibly update the block as a result. *)
      let possibly_updated_other_exprs_block =
        let other_exprs_block = T.Block (loc, type_expr, other_exprs) in
        match expr.node with
        | T.TLet (var_type, var_name, _) -> (
          match var_type with
          | Some (A.TEStruct (var_struct)) ->
            if E.struct_has_mode var_struct A.Linear env then
              type_linear_object_references var_name var_struct env
                other_exprs_block
            else other_exprs_block
          | _                      -> other_exprs_block
        )
        | _ -> other_exprs_block in
      (* recurse on the subsequent expressions in the block *)
      type_linear_capabilities_block_expr env possibly_updated_other_exprs_block
      >>| fun (Block (_, _, updated_other_exprs)) ->
      T.Block (loc, type_expr, expr :: updated_other_exprs)
    