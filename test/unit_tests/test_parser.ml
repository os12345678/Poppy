open Alcotest
open Poppy_parser.Ast

let test_parse_int_literal () =
  let input = "123" in
  let expected_output = IntLiteral 123 in
  let actual_output = Parser.parse input in
  check (option expr) "parses int literal correctly" (Some expected_output) actual_output