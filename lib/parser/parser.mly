%{
  open Ast

  exception Parse_error of string
%}

%token <int> INT
%token <string> ID
%token <string> TYPE
%token <string> STRING
%token LET
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIV
%token LT LEQ GT GEQ EQ NEQ
%token AND OR NOT XOR
%token IF ELSE
%token WHILE FOR
%token ASSIGN
%token COLON SEMICOLON
%token FN MAIN
%token RETURN
%token COMMA
%token LAMBDA ARROW
%token EOF

%type <Ast.expr> expr
%type <Ast.statement> statement
%type <Ast.statement list> statements
%type <Ast.expr list> args
%type <Ast.func_param list> params
%type <Ast.expr> atom_expr
%type <Ast.statement> expr_statement
%type <Ast.incr_decr_op> increment
%type <Ast.type_decl> typ_decl


%nonassoc EQ NEQ
%nonassoc LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV
%left AND OR XOR
%right NOT


%start main
%type <Ast.statement list> main
%%

main:
  | statements EOF
    {
      let main_found =
        List.exists (function
          | MainFunc _ -> true
          | _ -> false
        ) $1
      in
      if main_found then $1
      else raise (Parse_error "main function entrypoint not found!")
    }

main_func:
  | FN MAIN LPAREN RPAREN ARROW typ_decl LBRACE statements RBRACE { if $6 = Type Void then MainFunc ($8) else raise (Parse_error "Invalid main function identifier") }

statement:
  | LET ID COLON typ_decl ASSIGN expr SEMICOLON { Let((Id $2, $4), $6) }
  | ID ASSIGN expr SEMICOLON { Assign($1, $3) }
  | IF LPAREN expr RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE { If($3, Block($6), Block($10)) }
  | WHILE LPAREN expr RPAREN LBRACE statements RBRACE { While($3, Block($6)) }
  | FOR LPAREN ID ASSIGN INT COMMA expr COMMA increment RPAREN LBRACE statements RBRACE { For($3, $5, $7, $9, Block($12)) }
  | FN ID LPAREN params RPAREN ARROW typ_decl LBRACE statements RBRACE { FuncDecl (Id $2, $4, $7, $9) }
  | RETURN expr SEMICOLON { Return($2) }

expr_statement:
  | expr SEMICOLON { Expr($1) }

params: 
  | { [] }
  | ID COLON typ_decl { [Param (Id $1, $3)] }
  | ID COLON typ_decl COMMA params { Param(Id $1, $3) :: $5 }

increment:
  | ID PLUS PLUS { Incr($1) }
  | ID MINUS MINUS { Decr($1) }

statements:
  | main_func statements { 
      if List.exists (function MainFunc _ -> true | _ -> false) $2
      then raise (Parse_error "Multiple main functions found!");
      $1 :: $2 }
  | statement statements { $1 :: $2 }
  | expr_statement statements { $1 :: $2 }
  | { [] }

typ_decl:
  | TYPE { Type (string_to_typ $1) }

expr:
  | atom_expr                 { $1 }
  | expr PLUS expr            { BinOp (Plus, $1, $3) }
  | expr MINUS expr           { BinOp (Minus, $1, $3) }
  | expr TIMES expr           { BinOp (Times, $1, $3) }
  | expr DIV expr             { BinOp (Div, $1, $3) }
  | expr AND expr             { BinOp (And, $1, $3) }
  | expr OR expr              { BinOp (Or, $1, $3) }
  | expr XOR expr             { BinOp (Xor, $1, $3) }
  | NOT expr                  { Not $2 }
  | expr LT expr              { BinOp (Lt, $1, $3) }
  | expr LEQ expr             { BinOp (Leq, $1, $3) }
  | expr GT expr              { BinOp (Gt, $1, $3) }
  | expr GEQ expr             { BinOp (Geq, $1, $3) }
  | expr EQ expr              { BinOp (Eq, $1, $3) }
  | expr NEQ expr             { BinOp (Neq, $1, $3) }
  | LPAREN expr RPAREN        { $2 }
  | ID LPAREN args RPAREN     { Call ($1, $3) }
  | LAMBDA LPAREN params RPAREN ARROW LPAREN expr RPAREN { Lambda ($3, $7) }

atom_expr:
  | ID                          { Id $1 }
  | INT                         { IntLiteral $1 }
  | TRUE                        { BoolLiteral true }
  | FALSE                       { BoolLiteral false }
  | STRING                      { StringLiteral $1 }

args:
  | { [] }
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }
