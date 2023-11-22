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
  | Ok prog -> print_s [%sexp (print_program prog : string)]; [%expect.unreachable]
  | Error err -> print_s [%sexp (Error.to_string_hum err : string)]; [%expect{|
     "File \"\", line 2, characters 10-11:\
    \n: syntax error\
    \n" |}]
;;

