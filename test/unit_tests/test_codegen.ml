open Poppy_codegen.Codegen
open Poppy_parser.Parser_interface

let%expect_test "codegen_expr" =
let input = "fn main() -> int { 1 + 2; }" in
let ast = parse_input input in
let llvm_ir_string = codegen_ast_to_string ast in
print_endline (llvm_ir_string);
[%expect {|
  Generated LLVM IR:
  ; ModuleID = 'poppy_compiler'
  source_filename = "poppy_compiler"

  define i64 @main() {
  entry:
    ret i64 0
  } |}]

let%expect_test "codegen_fn_application" = 
let input = "
fn mult(a: int, b: int) -> int {
  return a * b;
}
fn main() -> int {
  mult(2, 3);
}" in
let ast = parse_input input in
let llvm_ir_string = codegen_ast_to_string ast in
print_endline (llvm_ir_string);
[%expect {|
  Generated LLVM IR:
  ; ModuleID = 'poppy_compiler'
  source_filename = "poppy_compiler"

  define i64 @mult(i64 %a, i64 %b) {
  entry:
    %multmp = mul i64 %a, %b
    ret i64 %multmp
  } 

  define i64 @main() {
  entry:
    %calltmp = call i64 @mult(i64 2, i64 3)
    ret i64 0
  } |}]

let%expect_test "codegen_multiple_fn_application" =
let input = "
fn mult(a: int, b: int) -> int {
  return a * b;
}
fn add(a: int, b: int) -> int {
  return a + b;
}
fn main() -> int {
  mult(2, 3);
  add(2, 3);
}" in
let ast = parse_input input in
let llvm_ir_string = codegen_ast_to_string ast in
print_endline (llvm_ir_string); 
[%expect {|
  Generated LLVM IR:
  ; ModuleID = 'poppy_compiler'
  source_filename = "poppy_compiler"

  define i64 @mult(i64 %a, i64 %b) {
  entry:
    %multmp = mul i64 %a, %b
    ret i64 %multmp
  }

  define i64 @add(i64 %a, i64 %b) {
  entry:
    %addtmp = add i64 %a, %b
    ret i64 %addtmp
  }

  define i64 @main() {
  entry:
    %calltmp = call i64 @mult(i64 2, i64 3)
    %calltmp1 = call i64 @add(i64 2, i64 3)
    ret i64 0
  } |}]