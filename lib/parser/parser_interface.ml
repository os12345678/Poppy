exception ParseError of string

let parse_input input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.main Lexer.read_tok lexbuf
  with
  | Lexer.SyntaxError msg ->
      let pos = lexbuf.lex_curr_p in
      raise (ParseError (Printf.sprintf "%s at line %d, column %d" msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      raise (ParseError (Printf.sprintf "Unexpected token '%s' at line %d, column %d" (Lexing.lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))

open Sexplib
let to_string (stmts : Ast.statement list) : string =
  stmts |> List.map Ast.sexp_of_statement
        |> List.map Sexp.to_string_hum
        |> String.concat "\n"