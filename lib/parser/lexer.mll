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

(* Regexes for tokens *)
let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*
let generic_type_param =  ['A' -'Z']

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
  (* | "::" { DOUBLECOLON } *)
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
  | "new" { NEW }
  | "const" {CONST }
  | "var" { VAR }
  | "struct" { STRUCT }
  (* | "type" { TYPE } *)
  | "interface" { INTERFACE }
  | "fn" { FUNCTION }
  (* | "consume" { CONSUME } *)
  (* | "finish" { FINISH } *)
  (* | "async" { ASYNC } *)
  (* | "class" { CLASS } *)
  (* | "extends" {EXTENDS} *)
  | generic_type_param { GENERIC_TYPE }
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
  | whitespace { read_tok lexbuf }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment 1 lexbuf } 
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
    (* | '"'      { read_string (Buffer.create 17) lexbuf } *)
  | newline { advance_line lexbuf; read_tok lexbuf }
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
