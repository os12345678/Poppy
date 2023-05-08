open Llvm
open Poppy_parser.Ast
open Scoping

(* ############################# LLVM setup ################################# *)
let context = global_context ()
let builder = builder context
let the_module = create_module context "Poppy"

(* ############################# LLVM types ################################# *)
let rec llvm_type_of_typ context = function
  | Int -> i64_type context
  | Bool -> i1_type context
  | Void -> void_type context
  | String -> pointer_type (i8_type context)
  | Function (param_types, return_type) ->
    let llvm_param_types = Array.of_list (List.map (fun typ -> llvm_type_of_typ context typ) param_types) in
    let llvm_return_type = llvm_type_of_typ context return_type in
    function_type llvm_return_type llvm_param_types
  | _ -> raise (Failure (Printf.sprintf "llvm_type_of_typ: ClassInstance not supported yet"))

(* ###################### Helper functions for codegen ###################### *)

let func_map : (string, Llvm.llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10
let is_pointer (llvm_type: lltype) : bool =
  classify_type llvm_type = TypeKind.Pointer

  let find_function (function_map: (string, llvalue) Hashtbl.t) (func_name: string) : llvalue option =
    try
      Some (Hashtbl.find function_map func_name)
    with
      Not_found -> None

(* ####################### Auxiliary Codegen Expr ########################### *)
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

let codegen_call func_name args func_map =
  let func = match find_function func_map func_name with
    | Some func -> func
    | None -> raise (Failure (Printf.sprintf "Function %s not found" func_name))
  in
  let params = params func in
  let args = List.map2 (fun param arg ->
    let param_type = type_of param in
    let arg_type = type_of arg in
    if param_type = arg_type then arg
    else if is_pointer param_type && arg_type = i64_type context then
      build_inttoptr arg param_type "inttoptrtmp" builder
    else if param_type = i64_type context && is_pointer arg_type then
      build_ptrtoint arg param_type "ptrtointtmp" builder
    else raise (Failure (Printf.sprintf "Type mismatch in function call %s" func_name))
  ) (Array.to_list params) args in
  build_call func (Array.of_list args) "calltmp" builder
  