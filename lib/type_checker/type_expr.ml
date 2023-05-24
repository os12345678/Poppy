open Poppy_parser.Ast_types
open Poppy_parser
open Type_env
open Core  
(* open Typed_ast *)

let type_identifier (_struct_defn: Ast.struct_defn) (_function_defn: Ast.function_defn) 
  (id: Ast.identifier) (ctx: context) (loc: loc) =
  let open Result in
  match id with
  | Ast.Variable var ->
    get_var_type ctx var loc
      >>| fun t -> (Typed_ast.TVariable (var, t), t)
  | Ast.ObjField (_var, _field_name) -> Or_error.error_string "ObjField not implemented"

let rec type_expr (struct_defn: Ast.struct_defn) (interface_defn: Ast.interface_defn) (function_defn: Ast.function_defn) 
  (expr: Ast.expr) (ctx: context) =
  let open Result in 
  let _type_with_defns = type_expr struct_defn interface_defn function_defn in
  (* let type_block_with_defns = type_block_expr struct_defn function_defn in *)
  match expr.node with
  | Ast.Int i -> Ok ({Typed_ast.loc = expr.loc; typ = TEInt; node = TInt i})
  | Ast.Boolean b -> Ok ({Typed_ast.loc = expr.loc; typ = TEBool; node = TBoolean b})
  | Ast.Identifier id ->
    type_identifier struct_defn function_defn id ctx expr.loc
    >>| fun (typed_id, id_type) -> {Typed_ast.loc = expr.loc; typ = id_type; node = TIdentifier typed_id}
  | Ast.Let (_type_annot_maybe, _var_name, _expr)-> Or_error.error_string "Let not implemented"

  | _ -> Or_error.error_string "Not implemented"

(* and type_block_expr (struct_defn: Ast.struct_defn) (interface_defn: Ast.interface_defn) (function_defn: Ast.function_defn) 
  (block: Ast.block_expr) (_ctx: context) =
  let open Result in
  let _type_expr_with_defns = type_expr struct_defn interface_defn function_defn in
  let _type_block_expr_with_defns = type_block_expr struct_defn interface_defn function_defn in
  match block with
  | _ -> Or_error.error_string "Not implemented" *)

