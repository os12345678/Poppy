{
open Parser

let advance_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let pos' = { pos with
    Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
  } in
  lexbuf.Lexing.lex_curr_p <- pos'
}



(* Helper Regexes *)
let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* Rules *)

rule read_tok = 
  parse
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  (* Arithmetic operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  (* Comparison operators *)
  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  | "==" { EQ }
  | "!=" { NEQ }
  (* Logical operators *)
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  (* Keywords *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  (* Symbols *)
  | "=" { ASSIGN }
  (* Identifiers and constants *)
  | "True" { TRUE }
  | "False" { FALSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* Other *)
  | whitespace { read_tok lexbuf }
  | newline { advance_line lexbuf; read_tok lexbuf }
  | eof { EOF }
  | _ { raise (Failure "Invalid token") }