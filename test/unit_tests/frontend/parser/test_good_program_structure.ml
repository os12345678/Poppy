open Poppy_parser.Lex_and_parse
open Core 

let%expect_test "parse smallest program" =
  let lexbuf = Lexing.from_string "void main() {}" in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {| "(Prog () () () (Block ((lnum 1) (cnum 13)) ()))" |}]
;;

let%expect_test "parse main with function" = 
  let lexbuf = Lexing.from_string 
  "
  fn int foo() {
    1
  }

  void main() {}
  "
  in 
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
     "(Prog () ()\
    \n ((TFunction foo () TEInt ()\
    \n   (Block ((lnum 2) (cnum 16)) (((loc ((lnum 3) (cnum 5))) (node (Int 1)))))))\
    \n (Block ((lnum 6) (cnum 15)) ()))" |}]
;;

let%expect_test "parse program with struct and interface" = 
  let lexbuf = Lexing.from_string
  "
    struct Point { 
      capability linear linearPoint; 
      var int x:linearPoint;
      var int y:linearPoint;
      distanceToOriginz borrowed int() {
          let d:int = x*x + y*y;
          d
      }
    }

    interface hasDistance { 
        distanceToOrigin borrowed int();
    }

    fn borrowed void printDistance (borrowed struct hasDistance d){ 
        let distance:int = d.distanceToOrigin(); 
        // imaginary to-be-implemented print function
    }

    void main() {}
  "
  in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
     "(Prog\
    \n ((TStruct Point ((TCapability Linear linearPoint))\
    \n   ((TField MVar TEInt x (linearPoint)) (TField MVar TEInt y (linearPoint)))\
    \n   ((TMethod distanceToOriginz (Borrowed) TEInt ()\
    \n     (Block ((lnum 6) (cnum 40))\
    \n      (((loc ((lnum 7) (cnum 11)))\
    \n        (node\
    \n         (Let (TEInt) d\
    \n          ((loc ((lnum 7) (cnum 23)))\
    \n           (node\
    \n            (BinOp BinOpPlus\
    \n             ((loc ((lnum 7) (cnum 23)))\
    \n              (node\
    \n               (BinOp BinOpMult\
    \n                ((loc ((lnum 7) (cnum 23))) (node (Identifier (Variable x))))\
    \n                ((loc ((lnum 7) (cnum 25))) (node (Identifier (Variable x)))))))\
    \n             ((loc ((lnum 7) (cnum 29)))\
    \n              (node\
    \n               (BinOp BinOpMult\
    \n                ((loc ((lnum 7) (cnum 29))) (node (Identifier (Variable y))))\
    \n                ((loc ((lnum 7) (cnum 31))) (node (Identifier (Variable y)))))))))))))\
    \n       ((loc ((lnum 8) (cnum 11))) (node (Identifier (Variable d))))))))))\
    \n ((TInterface hasDistance\
    \n   ((TInterfaceMethod distanceToOrigin (Borrowed) TEInt ()))))\
    \n ((TFunction printDistance (Borrowed) TEVoid\
    \n   ((Param (TEStruct hasDistance) d () (Borrowed)))\
    \n   (Block ((lnum 16) (cnum 67))\
    \n    (((loc ((lnum 17) (cnum 9)))\
    \n      (node\
    \n       (Let (TEInt) distance\
    \n        ((loc ((lnum 17) (cnum 28)))\
    \n         (node (MethodApp d distanceToOrigin ()))))))))))\
    \n (Block ((lnum 21) (cnum 17)) ()))" |}]
;;