open Alcotest
open Poppy_parser.Ast
open Poppy_parser.Parser_interface
open Sexplib

module Testable = struct
  let cmp_expr e1 e2 = match (e1, e2) with
    | IntLiteral i1, IntLiteral i2 -> i1 = i2
    | _ -> false

  let equal_statement s1 s2 = match (s1, s2) with
    | Expr e1, Expr e2 -> cmp_expr e1 e2
    | _ -> false

  let statement = Alcotest.testable
      (fun ppf s ->
         Fmt.pf ppf "%s" (Sexp.to_string_hum (sexp_of_statement s)))
      equal_statement
end


let test_parse_empty_void_fn () =
  let input = "
  fn main() -> void {
  }
  " 
  in
  let expected_output = [FuncDecl (Id "main", [], Type Void, [])] in
  let actual_output = parse_input input in
  check (list Testable.statement) "parses empty void fn correctly" expected_output actual_output


  let suite =
    [
      "test_parse_int_literal", `Quick, test_parse_empty_void_fn;
      (* Add more test cases here *)
    ]
  
  let () = Alcotest.run "Poppy parser tests" [("parser", suite)]
