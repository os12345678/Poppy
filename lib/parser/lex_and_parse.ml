open Core
open Lexer 
open Lexing 
open Ast

let print_err_pos lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "File \"%s\", line %d, characters %d-%d:\n"
    pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf = 
  try Ok (Parser.program Lexer.read_tok lexbuf) with
  | SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_err_pos lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg = Fmt.str "%s: syntax error@." (print_err_pos lexbuf) in
      Error (Error.of_string error_msg)

let print_program (prog: program) : string = 
  let sexp = Ast.sexp_of_program prog in
  Sexp.to_string_hum sexp
      