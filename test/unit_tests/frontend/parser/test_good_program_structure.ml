open Poppy_parser.Lex_and_parse
open Core 

let%expect_test "parse smallest program" =
  let lexbuf = Lexing.from_string "void main() {}" in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect {| "(Prog () () () () (Block ((lnum 1) (cnum 13)) ()))" |}]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect.unreachable]
;;

let%expect_test "parse full function" = 
  let lexbuf = Lexing.from_string 
  "
  struct Point { 
    capability linear linearCap, subordinate subCap;
    const int x: Read;
    const int y: Read;
}

trait Movable {
    move borrowed : linearCap, subCap(int dx, int dy) -> void
    move2 : subCap(int dx, int dy) -> void
}

impl Movable for Point {
    move borrowed : linearCap, subCap(int dx, int dy) -> void {
        this.x := this.x + dx; 
        this.y := this.y + dy 
    }
}

fn add(int a, int b) -> int {
    a + b
}

void main() {
    new p = Point { x: 1, y: 2};
    p.move(3, 4);
    let sum = add(p.x, p.y) //not working
}
  "
  in 
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect{|
     "(Prog\
    \n ((TStruct Point\
    \n   ((TCapability Linear linearCap) (TCapability Subordinate subCap))\
    \n   ((TField MConst TEInt x (Read)) (TField MConst TEInt y (Read)))))\
    \n ((TTrait Movable\
    \n   ((TMethodSignature move (Borrowed) (linearCap subCap)\
    \n     ((Param TEInt dx () ()) (Param TEInt dy () ())) TEVoid)\
    \n    (TMethodSignature move2 () (subCap)\
    \n     ((Param TEInt dx () ()) (Param TEInt dy () ())) TEVoid))))\
    \n ((TImpl Movable Point\
    \n   ((TMethod\
    \n     (TMethodSignature move (Borrowed) (linearCap subCap)\
    \n      ((Param TEInt dx () ()) (Param TEInt dy () ())) TEVoid)\
    \n     (Block ((lnum 14) (cnum 63))\
    \n      (((loc ((lnum 15) (cnum 9)))\
    \n        (node\
    \n         (Assign (ObjField this x)\
    \n          ((loc ((lnum 15) (cnum 19)))\
    \n           (node\
    \n            (BinOp BinOpPlus\
    \n             ((loc ((lnum 15) (cnum 19)))\
    \n              (node (Identifier (ObjField this x))))\
    \n             ((loc ((lnum 15) (cnum 28))) (node (Identifier (Variable dx))))))))))\
    \n       ((loc ((lnum 16) (cnum 9)))\
    \n        (node\
    \n         (Assign (ObjField this y)\
    \n          ((loc ((lnum 16) (cnum 19)))\
    \n           (node\
    \n            (BinOp BinOpPlus\
    \n             ((loc ((lnum 16) (cnum 19)))\
    \n              (node (Identifier (ObjField this y))))\
    \n             ((loc ((lnum 16) (cnum 28))) (node (Identifier (Variable dy))))))))))))))))\
    \n ((TFunction add () TEInt ((Param TEInt a () ()) (Param TEInt b () ()))\
    \n   (Block ((lnum 20) (cnum 29))\
    \n    (((loc ((lnum 21) (cnum 5)))\
    \n      (node\
    \n       (BinOp BinOpPlus\
    \n        ((loc ((lnum 21) (cnum 5))) (node (Identifier (Variable a))))\
    \n        ((loc ((lnum 21) (cnum 9))) (node (Identifier (Variable b)))))))))))\
    \n (Block ((lnum 24) (cnum 13))\
    \n  (((loc ((lnum 25) (cnum 5)))\
    \n    (node\
    \n     (Constructor p Point\
    \n      ((ConstructorArg x ((loc ((lnum 25) (cnum 24))) (node (Int 1))))\
    \n       (ConstructorArg y ((loc ((lnum 25) (cnum 30))) (node (Int 2))))))))\
    \n   ((loc ((lnum 26) (cnum 5)))\
    \n    (node\
    \n     (MethodApp p move\
    \n      (((loc ((lnum 26) (cnum 12))) (node (Int 3)))\
    \n       ((loc ((lnum 26) (cnum 15))) (node (Int 4)))))))\
    \n   ((loc ((lnum 27) (cnum 5)))\
    \n    (node\
    \n     (Let () sum\
    \n      ((loc ((lnum 27) (cnum 15)))\
    \n       (node\
    \n        (FunctionApp add\
    \n         (((loc ((lnum 27) (cnum 19))) (node (Identifier (ObjField p x))))\
    \n          ((loc ((lnum 27) (cnum 24))) (node (Identifier (ObjField p y))))))))))))))" |}]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect.unreachable]
;;

