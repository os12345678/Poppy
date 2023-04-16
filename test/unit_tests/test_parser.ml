open Poppy_parser.Parser_interface

let%expect_test "parse_empty_fn" =
  let input = "fn main() -> void {}" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Void) ()) |}]

let%expect_test "parse_empty_fn_with_args" =
  let input = "fn main(a: int, b: int) -> void {}" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) ((Param (Id a) (Type Int)) (Param (Id b) (Type Int)))
     (Type Void) ()) |}]

let%expect_test "parse_int" = 
  let input = "fn main() -> void { 123; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Void) ((Expr (IntLiteral 123)))) |}]

let%expect_test "parse_expr" =
  let input = "fn main() -> void { 1 + 2; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) () (Type Void)
     ((Expr (BinOp Plus (IntLiteral 1) (IntLiteral 2))))) |}]

let%expect_test "parse_function_call" =
  let input = "fn main() -> void { foo(); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Void) ((Expr (Call foo ())))) |}]

let%expect_test "parse_function_call_with_args" =
  let input = "fn main() -> void { foo(1, 2); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) () (Type Void)
     ((Expr (Call foo ((IntLiteral 1) (IntLiteral 2)))))) |}]

let%expect_test "function_application" = 
  let input = "
    fn f(x: int) -> int {
      x;
    }
    fn main() -> void {
      f(1);
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id f) ((Param (Id x) (Type Int))) (Type Int) ((Expr (Id x))))
    (FuncDecl (Id main) () (Type Void) ((Expr (Call f ((IntLiteral 1)))))) |}]

