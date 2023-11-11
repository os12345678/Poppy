open Poppy_parser.Lex_and_parse
open Core 

let%expect_test "Access local variable from multiple locals" =
  let lexbuf = Lexing.from_string 
  "
  struct Foo{
    capability local Bar;
    var int f : Bar;
  }
  void main(){
    new x = Foo{f:0};
    finish {
      async{
        x.f := 1
      }
      let y = x;
      y.f
    }
  }


  " in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect{|
     "(Prog\
    \n ((TStruct Foo ((TCapability ThreadLocal Bar)) ((TField MVar TEInt f (Bar)))))\
    \n () () ()\
    \n (Block ((lnum 6) (cnum 14))\
    \n  (((loc ((lnum 7) (cnum 5)))\
    \n    (node\
    \n     (Constructor x Foo\
    \n      ((ConstructorArg f ((loc ((lnum 7) (cnum 19))) (node (Int 0))))))))\
    \n   ((loc ((lnum 8) (cnum 5)))\
    \n    (node\
    \n     (FinishAsync\
    \n      ((AsyncExpr\
    \n        (Block ((lnum 9) (cnum 12))\
    \n         (((loc ((lnum 10) (cnum 9)))\
    \n           (node\
    \n            (Assign (ObjField x f)\
    \n             ((loc ((lnum 10) (cnum 16))) (node (Int 1))))))))))\
    \n      (Block ((lnum 8) (cnum 5))\
    \n       (((loc ((lnum 12) (cnum 7)))\
    \n         (node\
    \n          (Let () y\
    \n           ((loc ((lnum 12) (cnum 15))) (node (Identifier (Variable x)))))))\
    \n        ((loc ((lnum 13) (cnum 7))) (node (Identifier (ObjField y f))))))))))))" |}]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect.unreachable]
;;