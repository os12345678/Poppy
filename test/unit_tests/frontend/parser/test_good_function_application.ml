open Poppy_parser.Lex_and_parse
open Core 

let%expect_test "parse function application" =
  let lexbuf = Lexing.from_string 
  "
    fn int foo(int x) {
      (x + 1)
    }

    void main() {
        foo(354);
    }
  " in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
     "(Prog () ()\
    \n ((TFunction foo () TEInt ((Param TEInt x () ()))\
    \n   (Block ((lnum 2) (cnum 23))\
    \n    (((loc ((lnum 3) (cnum 8)))\
    \n      (node\
    \n       (BinOp BinOpPlus\
    \n        ((loc ((lnum 3) (cnum 8))) (node (Identifier (Variable x))))\
    \n        ((loc ((lnum 3) (cnum 12))) (node (Int 1))))))))))\
    \n (Block ((lnum 6) (cnum 17))\
    \n  (((loc ((lnum 7) (cnum 9)))\
    \n    (node (FunctionApp foo (((loc ((lnum 7) (cnum 13))) (node (Int 1))))))))))" |}]
;;

