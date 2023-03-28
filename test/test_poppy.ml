open Core
open Poppy

let test_case input expected =
  let result = Poppy.Parser.main Poppy.Lexer.read_tok (Lexing.from_string input) in
  if Poly.equal result expected
  then print_endline "Test passed."
  else (
    print_endline "Test failed:";
    print_endline ("Input: " ^ input);
    print_endline ("Expected: " ^ (Sexp.to_string_hum (Ast.sexp_of_statement expected)));
    print_endline ("Result: " ^ (Sexp.to_string_hum (Ast.sexp_of_statement result)))
  )

let () =
  let input = "x = 3 + 5" in
  let expected = Poppy.Ast.Assign ("x", BinOp (Plus, Int 3, Int 5)) in
  test_case input expected
