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
let newline = '\r' | '\n' | "\r\n"

(* Rules *)
rule read_tok = 
  parse
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT } 
  | "/" { DIV }
  | "%" { REM }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "&&" { AND }
  | "||" { OR }
  | "!" { EXCLAMATION_MARK }
  | ":=" { COLONEQ }
  | "let" { LET }
  | "const" {CONST }
  | "var" { VAR }
  | "struct" { STRUCT }
  | "trait" { TRAIT }
  | "impl" { IMPL }
  | "fn" { FUNCTION }
  | "->" { ARROW }
  | "new" { NEW }
  | "mutex" { MUTEX }
  | "lock" { LOCK }
  | "unlock" { UNLOCK }
  | "create_thread" { CREATE_THREAD }
  | "capability" { CAPABILITY }
  | "linear" { LINEAR }
  | "local" { LOCAL }
  | "read" { READ }
  | "subordinate" { SUBORDINATE }
  | "locked" { LOCKED }
  | "int" { TYPE_INT }
  | "bool" { TYPE_BOOL } 
  | "void" { TYPE_VOID }
  | "borrowed" { BORROWED }
  | "true" { TRUE }
  | "false" { FALSE }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "main" { MAIN }
  | "printf" { PRINTF }
  | whitespace { read_tok lexbuf }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment 1 lexbuf } 
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
  | newline { advance_line lexbuf; read_tok lexbuf }
  | '"' { read_string (Buffer.create 17) lexbuf }
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
