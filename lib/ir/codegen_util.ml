open Llvm
open Poppy_parser
open Scoping

(* ############################# LLVM setup ################################# *)
let context = global_context ()
let builder = builder context
let the_module = create_module context "Poppy"

let string_of_llmodule m =
  let s = string_of_llmodule m in
  dispose_module m;
  s

(* ############################# LLVM types ################################# *)
let rec llvm_type_of_typ context = function
| Ast.Int-> i64_type context
| Ast.Bool -> i1_type context
| Ast.Void -> void_type context (* void type not working *)
| Ast.String -> pointer_type (i8_type context) 
| Ast.Function (param_types, return_type) ->
  let llvm_param_types = Array.of_list (List.map (fun typ -> llvm_type_of_typ context typ) param_types) in
  let llvm_return_type = llvm_type_of_typ context return_type in
  function_type llvm_return_type llvm_param_types
| _ -> raise (Failure "ClassInstance llvm type not yet implemented!")

(* ############################# LLVM values ################################# *)
;;

(* ################### Helper Functions for Aux Functions ################### *)

let func_map : (string, Llvm.llvalue) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 10
let is_pointer (llvm_type: lltype) : bool =
  classify_type llvm_type = TypeKind.Pointer

let find_function (function_map: (string, llvalue) Hashtbl.t) (func_name: string) : llvalue option =
  try
    Some (Hashtbl.find function_map func_name)
  with
    Not_found -> None

let rec find_class_by_llvm_type (scope: scope) (llvm_type: lltype) : class_info option =
  let found_class = Hashtbl.fold (fun _ class_info acc ->
    if acc = None && Llvm.struct_name llvm_type = Some class_info.class_name then
      Some class_info
    else
      acc
  ) scope.class_table None in
  match found_class with
  | Some _ -> found_class
  | None ->
    match scope.parent with
    | Some parent_scope -> find_class_by_llvm_type parent_scope llvm_type
    | None -> None

let find_class_member_llvm_value (class_info: class_info) (member_name: string) (instance_value: llvalue) : llvalue option =
  let member_index = ref None in
  let index = ref 0 in
  let _ = Hashtbl.iter (fun name _ ->
    if name = member_name then member_index := Some !index;
    index := !index + 1;
  ) class_info.member_variables in
  match !member_index with
  | Some index -> Some (build_struct_gep instance_value index (member_name ^ "_gep") builder)
  | None -> None

let is_return_statement (stmt: Ast.statement) : bool =
  match stmt with
  | Ast.Return _ -> true
  | _ -> false

let is_data_member = function 
| Ast.ClassVar _ -> true
| Ast.ClassMethod _ -> false

(* ####################### Auxiliary Codegen Expr ########################### *)
let find_named_value (id : string) (named_values : (string, llvalue) Hashtbl.t) : llvalue option =
  try
    Some (Hashtbl.find named_values id)
  with
    Not_found -> None

let codegen_binop op left right =
  match op with
  | Ast.Plus -> build_add left right "addtmp" builder
  | Ast.Minus -> build_sub left right "subtmp" builder
  | Ast.Times -> build_mul left right "multmp" builder
  | Ast.Div -> build_sdiv left right "divtmp" builder
  | Ast.Lt -> build_icmp Icmp.Slt left right "lttmp" builder
  | Ast.Gt -> build_icmp Icmp.Sgt left right "gttmp" builder
  | Ast.Leq -> build_icmp Icmp.Sle left right "leqtmp" builder
  | Ast.Geq -> build_icmp Icmp.Sge left right "geqtmp" builder
  | Ast.Eq -> build_icmp Icmp.Eq left right "eqtmp" builder
  | Ast.Neq -> build_icmp Icmp.Ne left right "neqtmp" builder
  | Ast.And -> build_and left right "andtmp" builder
  | Ast.Or -> build_or left right "ortmp" builder
  | Ast.Xor -> build_xor left right "xortmp" builder

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

let codegen_class_instantiation (current_scope: scope) (_var_name: string) (class_name: string) (llvm_args: llvalue list) (builder: llbuilder) : llvalue =
  match find_class current_scope class_name with
  | Some _class_info ->
    let constructor_name = class_name ^ "_init" in
      (match find_function func_map constructor_name with
      | Some constructor ->
        let instance = build_call constructor (Array.of_list llvm_args) "instancetmp" builder in
        instance
      | None -> raise (Failure (Printf.sprintf "Constructor %s not found" constructor_name)))
  | None -> raise (Failure (Printf.sprintf "Class %s not found" class_name))

let codegen_class_member_access (instance_value: llvalue) (member_name: string) (scope: scope) (_class_info: class_info) : llvalue =
  let instance_type = type_of instance_value in
  (match Llvm.classify_type instance_type with
  | TypeKind.Struct ->
    (match find_class_by_llvm_type scope instance_type with
    | Some found_class_info ->
      (match find_class_member_llvm_value found_class_info member_name instance_value with
      | Some member_value -> member_value
      | None -> raise (Failure (Printf.sprintf "ClassMemberAccess: Member %s not found in class %s" member_name found_class_info.class_name)))
    | None -> raise (Failure (Printf.sprintf "ClassMemberAccess: Class not found for LLVM type")))
  | _ -> raise (Failure "The expression is not an instance of a class"))

(* ####################### Auxiliary Codegen Expr ########################### *)

  

(* ########################### Core Library ################################# *)
let link_core_library the_module =
  let bindings = "/Users/oliver/Documents/University/Honours/poppy/core_lib/bindings.ll" in

  (* Parse the core library LLVM IR *)
  let context = global_context () in
  let corelib_buf = MemoryBuffer.of_file bindings in
  let corelib_module =
    try
      Llvm_irreader.parse_ir context corelib_buf
    with
    | Llvm_irreader.Error msg ->
      Printf.printf "Error parsing core library: %s\n" msg;
      raise (Failure "Error parsing core library")
    in
  Llvm.iter_functions (fun f -> Printf.printf "Core lib function: %s\n" (Llvm.value_name f)) corelib_module;

  (* Link the core library into the main module *)
  Llvm_linker.link_modules' the_module corelib_module;
  print_endline "Core library linked to the main module."