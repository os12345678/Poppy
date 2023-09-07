open Core

module T = Poppy_type_checker.Typed_ast
module Typ = Poppy_parser.Ast_types
module D = Desugared_ast

(* 
for each struct_defn in program:
    dstruct = {
        name: struct_name,
        fields: [list of fields in the struct]
    }
    add dstruct to dprogram.structs 
*)

let rec desugar_struct_defn (struct_defn: T.struct_defn) : D.dstruct =
  match struct_defn with
  | TStruct (struct_name, _, field_defns) ->
    let desugared_fields = List.map ~f:desugar_field_defn field_defns in
    {
      name = Typ.Struct_name.to_string struct_name;
      fields = desugared_fields;
    }

and desugar_field_defn (field_defn: Typ.field_defn) : (string * Typ.type_expr) =
  match field_defn with
  | TField (_, type_expr, field_name, _) ->
    (Typ.Field_name.to_string field_name, type_expr)