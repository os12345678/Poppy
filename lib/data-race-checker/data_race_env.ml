module T = Poppy_type_checker.Typed_ast
module A = Poppy_parser.Ast_types
module E = Poppy_type_checker.Type_env

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let var_lists_are_equal xs ys =
  let compare_fn x y = String.compare (A.Var_name.to_string x) (A.Var_name.to_string y) in
  let deduped_xs = Core.List.dedup_and_sort ~compare:compare_fn xs in
  let deduped_ys = Core.List.dedup_and_sort ~compare:compare_fn ys in
  List.equal (fun x y -> x = y) deduped_xs deduped_ys


let rec reduce_expr_to_obj_ids (expr: T.expr) =
  match expr.node with
  | T.TInt _ | T.TBoolean _ -> []
  | T.TIdentifier (id) -> [id]
  | T.TBlockExpr (block_expr) -> reduce_block_expr_to_obj_ids block_expr
  | T.TConstructor (_, _, _) -> []
  | T.TLet (var_type, var_name, _) ->
      [T.TVariable (var_name, Option.get var_type, [], None)]
  | T.TAssign (id, _) -> [id]
  (* | T.TConsume (_, _) -> [] *)
  | T.TMethodApp (_, _, _, _, _) -> []
  | T.TFunctionApp ( _, _) -> []
  | T.TPrintf (_, _) -> []
  | T.TFinishAsync (_, curr_thread_expr) ->
      reduce_block_expr_to_obj_ids curr_thread_expr
  | T.TIf (_, then_expr, else_expr) ->
      let then_id = reduce_block_expr_to_obj_ids then_expr in
      let else_id = reduce_block_expr_to_obj_ids else_expr in
      then_id @ else_id
  | T.TWhile _ -> []
  | T.TBinOp _ -> [] (* Bin op returns either a TEInt or a Bool *)
  | T.TUnOp _ -> []

and reduce_block_expr_to_obj_ids (Block (loc, type_expr, exprs)) =
  match exprs with
  | []             -> []
  | [expr]         -> reduce_expr_to_obj_ids expr
  | _ :: rem_exprs -> reduce_block_expr_to_obj_ids (Block (loc, type_expr, rem_exprs))


let capability_mode_present mode_present mode_required =
  match mode_required with
  | A.ThreadSafe -> (
    match mode_present with
    | A.Read | Locked | ThreadSafe -> true
    | Linear | ThreadLocal | Subordinate | Encapsulated -> false )
  | Encapsulated -> (
    match mode_present with
    | Subordinate | Encapsulated -> true
    | Linear | ThreadLocal | Read | Locked | ThreadSafe -> false )
  | Linear | ThreadLocal | Subordinate | Read | Locked -> mode_present = mode_required
     

let struct_has_mode struct_name mode env =
  let rec struct_has_mode_helper struct_name mode env seen_struct_names =
    if elem_in_list struct_name seen_struct_names then
      (* Avoid infinite recursion on type definition *)
      false
    else
      E.get_struct_capabilities struct_name env
      |> fun capabilities ->
      E.get_struct_fields struct_name env
      |> fun fields ->
      match mode with
      (* any one of its capabilities (and nested field types) hold the mode *)
      | A.Linear | Subordinate | ThreadLocal ->
          List.exists
            (fun (A.TCapability (capability_mode, _)) -> capability_mode = mode)
            capabilities
          || List.exists
               (fun (A.TField (_, field_type, _, _)) ->
                 match field_type with
                 | A.TEStruct (struct_name) ->
                     struct_has_mode_helper struct_name mode env
                       (struct_name :: seen_struct_names)
                 | _                         -> false)
               fields
      (* all its capabilities hold the mode *)
      | Read | Encapsulated | ThreadSafe ->
          List.for_all
            (fun (A.TCapability (capability_mode, _)) ->
              capability_mode_present capability_mode mode)
            capabilities
      | Locked ->
          struct_has_mode_helper struct_name ThreadSafe env seen_struct_names
          && List.exists
               (fun (A.TCapability (capability_mode, _)) -> capability_mode = Locked)
               capabilities in
  struct_has_mode_helper struct_name mode env []

let type_has_mode type_expr mode env =
  match type_expr with
  | A.TEStruct (struct_name) -> struct_has_mode struct_name mode env
  | _                       -> false

let get_identifier_name = function
  | T.TVariable (name, _, _, _) -> name
  | T.TObjField (_, name, _, _, _, _) -> name

let get_identifier_capabilities = function 
  | T.TVariable (_, _, caps, _) -> caps
  | T.TObjField (_, _, _, _, caps, _) -> caps

let identifier_matches_var_name var_name = function
| T.TVariable (name, _, _, _) -> name = var_name
| T.TObjField (_, name, _, _, _, _) -> name = var_name

let param_to_obj_var_and_capabilities env
    (A.Param (type_expr, param_name, maybe_capability_guards, _)) =
  match type_expr with
  | A.TEStruct param_struct ->
      let class_capabilities = E.get_struct_capabilities param_struct env in
      let obj_capabilities =
        match maybe_capability_guards with
        | None -> class_capabilities
        (* no constraints so can access anything *)
        | Some capability_guards ->
            Core.List.filter
              ~f:(fun (TCapability (_, cap_name)) ->
                elem_in_list cap_name capability_guards)
              class_capabilities in
      Some (param_name, param_struct, obj_capabilities)
  | _                        ->
      (* not an object so ignore *)
      None

let params_to_obj_vars_and_capabilities class_defns params =
  Core.List.filter_map ~f:(param_to_obj_var_and_capabilities class_defns) params

let set_identifier_capabilities id new_capabilities =
  match id with
  | T.TVariable (var_type, var_name, _, maybeBorrowed) ->
      T.TVariable (var_type, var_name, new_capabilities, maybeBorrowed)
  | T.TObjField (obj_class, obj_name, field_type, field_name, _, maybeBorrowed) ->
      T.TObjField
        (obj_class, obj_name, field_type, field_name, new_capabilities, maybeBorrowed)

let get_class_capability_fields struct_name capability_name env =
  E.get_struct_fields struct_name env
  |> fun fields ->
  List.filter
    (fun (A.TField (_, _, _, field_capability_names)) ->
      elem_in_list capability_name field_capability_names)
    fields

   
  
let capability_fields_have_mode (A.TCapability (capability_mode, capability_name))
  class_name mode env =
  capability_mode_present capability_mode mode
  || get_class_capability_fields class_name capability_name env
      |> fun fields_in_capability ->
      List.exists
        (fun (A.TField (_, field_type, _, _)) ->
          match field_type with
          | A.TEStruct (struct_name) -> struct_has_mode struct_name mode env
          | _                        -> false)
        fields_in_capability

(* Check if the expression is reduced to an id that matches a given name_to_match *)
  let has_matching_expr_reduced_ids should_match_fields name_to_match ids =
    List.length
      (Core.List.filter
         ~f:(function
           | T.TVariable (name, _, _, _) -> name_to_match = name
           | T.TObjField (_, obj_name, _, _, _, _) ->
               should_match_fields && name_to_match = obj_name)
         ids)
    > 0

let rec find_immediate_aliases_in_expr should_match_fields orig_obj_name curr_aliases expr =
  let find_imm_aliases_in_expr_rec =
      find_immediate_aliases_in_expr should_match_fields orig_obj_name in
    let find_imm_aliases_in_block_expr_rec =
      find_immediate_aliases_in_block_expr should_match_fields orig_obj_name in
    match expr with
    | T.TLet (_, name, bound_expr) ->
        find_immediate_aliases_in_expr should_match_fields orig_obj_name curr_aliases
          bound_expr.node
        |> fun updated_aliases ->
        reduce_expr_to_obj_ids bound_expr
        |> fun expr_reduced_ids ->
        if
          List.exists
            (fun name_to_match ->
              has_matching_expr_reduced_ids should_match_fields name_to_match
                expr_reduced_ids)
            (orig_obj_name :: updated_aliases)
        then name :: updated_aliases
        else updated_aliases
    (* now we just recurse in other cases *)
    | T.TInt _ | T.TBoolean _ | T.TIdentifier _ -> curr_aliases
    | T.TBlockExpr (block_expr) ->
        find_imm_aliases_in_block_expr_rec curr_aliases block_expr
    | T.TConstructor (_, _, constructor_args) ->
        Core.List.fold ~init:curr_aliases
          ~f:(fun acc_aliases (T.ConstructorArg (_, expr)) ->
            find_imm_aliases_in_expr_rec acc_aliases expr.node)
          constructor_args
    | T.TAssign (_, assigned_expr) ->
        find_imm_aliases_in_expr_rec curr_aliases assigned_expr.node
    (* | Consume _ -> curr_aliases *)
    | T.TMethodApp (_, _, _, _, args_exprs) ->
      let node_list = Core.List.map ~f:(fun e -> e.node) args_exprs in
      Core.List.fold ~init:curr_aliases ~f:find_imm_aliases_in_expr_rec node_list
    | T.TFunctionApp (_, args_exprs) ->
      let node_list = Core.List.map ~f:(fun e -> e.node) args_exprs in
      Core.List.fold ~init:curr_aliases ~f:find_imm_aliases_in_expr_rec node_list
    | T.TPrintf (_, args_exprs) ->
      let node_list = Core.List.map ~f:(fun e -> e.node) args_exprs in
      Core.List.fold ~init:curr_aliases ~f:find_imm_aliases_in_expr_rec node_list
    | T.TFinishAsync (async_exprs, curr_thread_expr) ->
        Core.List.fold
          ~init:(find_imm_aliases_in_block_expr_rec curr_aliases curr_thread_expr)
          ~f:(fun acc_aliases (AsyncExpr (async_expr)) ->
            find_imm_aliases_in_block_expr_rec acc_aliases async_expr)
          async_exprs
    | T.TIf (cond_expr, then_expr, else_expr) ->
        Core.List.fold
          ~init:(find_imm_aliases_in_expr_rec curr_aliases cond_expr.node)
          ~f:find_imm_aliases_in_block_expr_rec [then_expr; else_expr]
    | T.TWhile (cond_expr, loop_expr) ->
        (* Note we check twice to simulate going through loop multiple times *)
        find_imm_aliases_in_expr_rec curr_aliases cond_expr.node
        |> fun curr_aliases_with_cond ->
        find_imm_aliases_in_block_expr_rec curr_aliases_with_cond loop_expr
    | T.TBinOp (_, expr1, expr2) ->
        Core.List.fold ~init:curr_aliases ~f:find_imm_aliases_in_expr_rec [expr1.node; expr2.node]
    | T.TUnOp (_, expr) -> find_imm_aliases_in_expr_rec curr_aliases expr.node
  

and find_immediate_aliases_in_block_expr should_match_fields orig_obj_name curr_aliases
  (T.Block (_, _, exprs)) =
  let node_list = Core.List.map ~f:(fun e -> e.node) exprs in
    Core.List.fold ~init:curr_aliases
    ~f:(find_immediate_aliases_in_expr should_match_fields orig_obj_name)
    node_list

let find_aliases_in_block_expr ~should_match_fields name_to_match block_expr =
  (* Get the least fixed point of an object's aliases *)
  let rec get_all_obj_aliases should_match_fields curr_aliases block_expr =
    find_immediate_aliases_in_block_expr should_match_fields name_to_match curr_aliases
      block_expr
    |> fun updated_aliases ->
    if var_lists_are_equal updated_aliases curr_aliases then curr_aliases
    else get_all_obj_aliases should_match_fields updated_aliases block_expr in
  get_all_obj_aliases should_match_fields [] block_expr