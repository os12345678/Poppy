open! Core
open Poppy_parser.Ast

type env = typ String.Map.t

let empty_env = String.Map.empty

let rec check_expr env = function
  | IntLiteral _ -> Int
  | BoolLiteral _ -> Bool
  | VoidType -> Void
  | StringType _ -> String
  | StringLiteral _ -> String
  | Id id ->
    (match String.Map.find env id with
     | Some typ -> typ
     | None -> failwith (Printf.sprintf "Undeclared identifier: %s" id))
  | _ -> failwith "Not implemented YET"

