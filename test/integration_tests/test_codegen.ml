(* Integration tests for LLVM IR code generation.
   These tests verify that Poppy source programs produce valid LLVM IR modules,
   and that the generated IR correctly declares/references external functions
   with the right names and signatures. *)

open Poppy_parser
open Poppy_type_checker
open Desugar
open Poppy_codegen
open Poppy_codegen.Codegen_expr
open Poppy_codegen.Codegen_util
open Data_race_checker.Type_data_race_program

(* Helper: compile a Poppy source string to an LLVM module *)
let compile_source source =
  let lexbuf = Lexing.from_string source in
  let the_fpm = Llvm.PassManager.create_function the_module in
  match Lex_and_parse.parse_program lexbuf with
  | Error e -> failwith (Printf.sprintf "Parse error: %s" (Core.Error.to_string_hum e))
  | Ok ast ->
    match Type_program.type_program ast with
    | Error e -> failwith (Printf.sprintf "Type error: %s" (Core.Error.to_string_hum e))
    | Ok (env, typed_ast) ->
      match type_data_race_program env typed_ast ~ignore_data_races:true with
      | Error e -> failwith (Printf.sprintf "Data race error: %s" (Core.Error.to_string_hum e))
      | Ok _ ->
        match Desugar_program.desugar_program typed_ast with
        | Error e -> failwith (Printf.sprintf "Desugar error: %s" (Core.Error.to_string_hum e))
        | Ok dprogram ->
          let sym_table = Ir_symbol_table.build_symbol_table dprogram in
          codegen_ast dprogram sym_table the_fpm

(* Test: Verify that thread_create is declared (not create_thread) *)
let test_thread_create_declared () =
  let _m = compile_source "void main() { 1 }" in
  match Llvm.lookup_function "thread_create" the_module with
  | Some _ -> Printf.printf "PASS: thread_create is declared in the module\n"
  | None -> failwith "FAIL: thread_create not found in the LLVM module"

(* Test: Verify that thread_join is declared (not join_thread) *)
let test_thread_join_declared () =
  match Llvm.lookup_function "thread_join" the_module with
  | Some _ -> Printf.printf "PASS: thread_join is declared in the module\n"
  | None -> failwith "FAIL: thread_join not found in the LLVM module"

(* Test: Verify that thread_create has exactly 3 parameters *)
let test_thread_create_params () =
  match Llvm.lookup_function "thread_create" the_module with
  | Some fn ->
    let ft = Llvm.element_type (Llvm.type_of fn) in
    let num_params = Array.length (Llvm.param_types ft) in
    if num_params = 3 then
      Printf.printf "PASS: thread_create has 3 parameters\n"
    else
      failwith (Printf.sprintf "FAIL: thread_create has %d parameters (expected 3)" num_params)
  | None -> failwith "FAIL: thread_create not found"

(* Test: Verify that thread_join has exactly 2 parameters *)
let test_thread_join_params () =
  match Llvm.lookup_function "thread_join" the_module with
  | Some fn ->
    let ft = Llvm.element_type (Llvm.type_of fn) in
    let num_params = Array.length (Llvm.param_types ft) in
    if num_params = 2 then
      Printf.printf "PASS: thread_join has 2 parameters\n"
    else
      failwith (Printf.sprintf "FAIL: thread_join has %d parameters (expected 2)" num_params)
  | None -> failwith "FAIL: thread_join not found"

(* Test: Verify that main function returns i32 *)
let test_main_returns_i32 () =
  match Llvm.lookup_function "main" the_module with
  | Some fn ->
    let ft = Llvm.element_type (Llvm.type_of fn) in
    let ret_type = Llvm.return_type ft in
    if Llvm.classify_type ret_type = Llvm.TypeKind.Integer then
      Printf.printf "PASS: main returns i32\n"
    else
      failwith "FAIL: main does not return an integer type"
  | None -> failwith "FAIL: main function not found"

(* Test: Verify the module validates successfully *)
let test_module_valid () =
  (try
    Llvm_analysis.assert_valid_module the_module;
    Printf.printf "PASS: LLVM module is valid\n"
  with e ->
    failwith (Printf.sprintf "FAIL: LLVM module validation failed: %s" (Printexc.to_string e)))

let () =
  Printf.printf "=== LLVM IR Linking Integration Tests ===\n";
  test_thread_create_declared ();
  test_thread_join_declared ();
  test_thread_create_params ();
  test_thread_join_params ();
  test_main_returns_i32 ();
  test_module_valid ();
  Printf.printf "=== All tests passed ===\n"
