open Llvm
open Poppy_parser.Ast
open Scoping
(* open Poppy_type_checker *)

let context = global_context ()
let builder = builder context

(* Helper functions for codegen *)
let is_pointer (llvm_type: lltype) : bool =
  classify_type llvm_type = TypeKind.Pointer
;;

(* Auxiliary functions for codegen *)
let find_named_value (id : string) (named_values : (string, llvalue) Hashtbl.t) : llvalue option =
  try
    Some (Hashtbl.find named_values id)
  with
    Not_found -> None

let codegen_binop op left right =
  match op with
  | Plus -> build_add left right "addtmp" builder
  | Minus -> build_sub left right "subtmp" builder
  | Times -> build_mul left right "multmp" builder
  | Div -> build_sdiv left right "divtmp" builder
  | Lt -> build_icmp Icmp.Slt left right "lttmp" builder
  | Gt -> build_icmp Icmp.Sgt left right "gttmp" builder
  | Leq -> build_icmp Icmp.Sle left right "leqtmp" builder
  | Geq -> build_icmp Icmp.Sge left right "geqtmp" builder
  | Eq -> build_icmp Icmp.Eq left right "eqtmp" builder
  | Neq -> build_icmp Icmp.Ne left right "neqtmp" builder
  | And -> build_and left right "andtmp" builder
  | Or -> build_or left right "ortmp" builder
  | Xor -> build_xor left right "xortmp" builder

let codegen_class_instantiation (current_scope: scope) (_var_name: string) (class_name: string) (_exprs: expr list) (codegen_class_instance: class_info -> llvalue) : llvalue =
  match find_class current_scope class_name with
  | Some class_info ->
    let instance = create_instance class_info in
    (match instance with
    | ClassInstance (instance_class_info, _) -> codegen_class_instance instance_class_info
    | _ -> raise (Failure (Printf.sprintf "Unexpected value type when creating instance of class %s" class_name)))
  | None -> raise (Failure (Printf.sprintf "Class %s not found" class_name))