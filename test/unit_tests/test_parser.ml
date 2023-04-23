open Poppy_parser.Parser_interface

let%expect_test "parse_empty_fn" =
  let input = "fn main() -> int { return 0; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Int) ((Return (IntLiteral 0)))) |}]

let%expect_test "parse_int" = 
  let input = "fn main() -> int { 123; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Int) ((Expr (IntLiteral 123)))) |}]

let%expect_test "parse_expr" =
  let input = "fn main() -> int { 1 + 2; }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) () (Type Int)
     ((Expr (BinOp Plus (IntLiteral 1) (IntLiteral 2))))) |}]

let%expect_test "parse_function_call" =
  let input = "fn main() -> int { foo(); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Int) ((Expr (Call foo ())))) |}]

let%expect_test "parse_function_call_with_args" =
  let input = "fn main() -> int { foo(1, 2); }" in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) () (Type Int)
     ((Expr (Call foo ((IntLiteral 1) (IntLiteral 2)))))) |}]

let%expect_test "function_expression_application" = 
  let input = "
    fn f(x: int) -> int {
      x;
    }
    fn main() -> int {
      f(1);
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id f) ((Param (Id x) (Type Int))) (Type Int) ((Expr (Id x))))
    (FuncDecl (Id main) () (Type Int) ((Expr (Call f ((IntLiteral 1)))))) |}]

let%expect_test "function_statement_application" = 
  let input = "
    fn f(x: int) -> bool {
      if(x < 5){
        return True;
      } else {
        return False;
      }
    }
    fn main() -> int {
      f(1);
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id f) ((Param (Id x) (Type Int))) (Type Bool)
     ((If (BinOp Lt (Id x) (IntLiteral 5)) (Block ((Return (BoolLiteral true))))
       (Block ((Return (BoolLiteral false)))))))
    (FuncDecl (Id main) () (Type Int) ((Expr (Call f ((IntLiteral 1)))))) |}]

let%expect_test "thread_creation" = 
  let input = "
    fn main() -> int {
      thread{
        // do something
      }
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Int) ((Thread (Block ())))) |}]

let%expect_test "define mutexes" = 
  let input = "
    fn main() -> int {
      let mut1:int = mutex;
      mut1::lock();
      mut1::unlock();
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {|
    (FuncDecl (Id main) () (Type Int)
     ((MutexDeclaration (MutexId mut1) (Type Int)) (MutexLock (MutexId mut1))
      (MutexUnlock (MutexId mut1)))) |}]

let%expect_test "test_print" = 
  let input = "
    fn main() -> int {
      print(1);
    }
  " in
  let output = parse_input input in
  print_endline (to_string output);
  [%expect {| (FuncDecl (Id main) () (Type Int) ((Print (IntLiteral 1)))) |}]