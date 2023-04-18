open Poppy_parser.Parser_interface

let%expect_test "parse_empty_fn" =
  let input = "fn main() -> void {}" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (MainFunc ()) |}]

let%expect_test "parse_int" = 
  let input = "fn main() -> void { 123; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (MainFunc ((Expr (IntLiteral 123)))) |}]

let%expect_test "parse_expr" =
  let input = "fn main() -> void { 1 + 2; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (MainFunc ((Expr (BinOp Plus (IntLiteral 1) (IntLiteral 2))))) |}]

let%expect_test "parse_function_call" =
  let input = "fn main() -> void { foo(); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (MainFunc ((Expr (Call foo ())))) |}]

let%expect_test "parse_function_call_with_args" =
  let input = "fn main() -> void { foo(1, 2); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (MainFunc ((Expr (Call foo ((IntLiteral 1) (IntLiteral 2)))))) |}]

let%expect_test "function_expression_application" = 
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
    (MainFunc ((Expr (Call f ((IntLiteral 1)))))) |}]

let%expect_test "function_statement_application" = 
  let input = "
    fn f(x: int) -> bool {
      if(x < 5){
        return True;
      } else {
        return False;
      }
    }
    fn main() -> void {
      f(1);
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id f) ((Param (Id x) (Type Int))) (Type Bool)
     ((If (BinOp Lt (Id x) (IntLiteral 5)) (Block ((Return (BoolLiteral true))))
       (Block ((Return (BoolLiteral false)))))))
    (MainFunc ((Expr (Call f ((IntLiteral 1)))))) |}]
