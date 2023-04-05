{
  open Parser

  exception SyntaxError of string

  let advance_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    let pos' = { pos with
      Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
    } in
    lexbuf.Lexing.lex_curr_p <- pos'
}

(* Helper Regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"

(* Rules *)

rule read_tok = 
  parse
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
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
  | "^" { XOR }
  (* Keywords *)
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | "fn" { FN }
  | "return" { RETURN }
  (* Symbols *)
  | "=" { ASSIGN }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "," { COMMA }
  (* Identifiers and constants *)
  | "let" { LET }
  | "True" { TRUE }
  | "False" { FALSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | '"'      { read_string (Buffer.create 17) lexbuf }  
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* Other *)
  | "print" { PRINT }
  | whitespace { read_tok lexbuf }
  | newline { advance_line lexbuf; read_tok lexbuf }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment 1 lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { advance_line lexbuf; read_tok lexbuf } 
  | eof { EOF }
  | _ { read_single_line_comment lexbuf } 
  
and read_multi_line_comment depth = parse
  | "*/" { if depth = 1 then read_tok lexbuf else read_multi_line_comment (depth - 1) lexbuf }
  | "/*" { read_multi_line_comment (depth + 1) lexbuf }
  | newline { advance_line lexbuf; read_multi_line_comment depth lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment depth lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }