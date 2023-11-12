open Poppy_parser.Lex_and_parse
open Core 

let%expect_test "parse function application" =
  let lexbuf = Lexing.from_string 
  "
  struct Foo {
    capability linear Bar;
    const int f : Bar;
    const int g : Bar; 
    const int h : Bar;
  }
  void main(){
      new x = Foo{f:4, g:5, h:6};
      let y = consume x // Consume linear variable 
  }
  " in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect{|
     "(Prog\
    \n ((TStruct Foo ((TCapability Linear Bar))\
    \n   ((TField MConst TEInt f (Bar)) (TField MConst TEInt g (Bar))\
    \n    (TField MConst TEInt h (Bar)))))\
    \n () () ()\
    \n (Block ((lnum 8) (cnum 14))\
    \n  (((loc ((lnum 9) (cnum 7)))\
    \n    (node\
    \n     (Constructor x Foo\
    \n      ((ConstructorArg f ((loc ((lnum 9) (cnum 21))) (node (Int 4))))\
    \n       (ConstructorArg g ((loc ((lnum 9) (cnum 26))) (node (Int 5))))\
    \n       (ConstructorArg h ((loc ((lnum 9) (cnum 31))) (node (Int 6))))))))\
    \n   ((loc ((lnum 10) (cnum 7)))\
    \n    (node\
    \n     (Let () y ((loc ((lnum 10) (cnum 15))) (node (Consume (Variable x))))))))))" |}]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect.unreachable]
;;

let%expect_test "parse function application" =
  let lexbuf = Lexing.from_string 
  "
  struct Foo {
    capability local Bar;
    var Baz f : Bar;
  }
 struct Baz {
     capability linear Fa;
     var int g : Fa;
  }
  void main(){
      new x = Foo{f:4};
      let y = consume x.f // Consume linear field of variable 

  }

  " in
  let result = parse_program lexbuf in
  match result with
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect{|
     "(Prog\
    \n ((TStruct Foo ((TCapability ThreadLocal Bar))\
    \n   ((TField MVar (TEStruct Baz) f (Bar))))\
    \n  (TStruct Baz ((TCapability Linear Fa)) ((TField MVar TEInt g (Fa)))))\
    \n () () ()\
    \n (Block ((lnum 10) (cnum 14))\
    \n  (((loc ((lnum 11) (cnum 7)))\
    \n    (node\
    \n     (Constructor x Foo\
    \n      ((ConstructorArg f ((loc ((lnum 11) (cnum 21))) (node (Int 4))))))))\
    \n   ((loc ((lnum 12) (cnum 7)))\
    \n    (node\
    \n     (Let () y ((loc ((lnum 12) (cnum 15))) (node (Consume (ObjField x f))))))))))" |}]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect.unreachable]
;;