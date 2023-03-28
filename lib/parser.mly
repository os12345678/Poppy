%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIV
%token LT LEQ GT GEQ EQ NEQ
%token AND OR NOT
%token IF ELSE
%token WHILE
%token ASSIGN
%token COLON
%token FN 
%token COMMA
%token EOF

%start main
%type <Ast.statement> main

%%

main:
  | statement EOF { $1 }

statement:
  | ID ASSIGN expr { Assign($1, $3) }
  | IF LPAREN expr RPAREN COLON statement ELSE COLON statement { If($3, $6, $9) }
  | WHILE LPAREN expr RPAREN COLON statement { While($3, $6) }
  | LBRACE statements RBRACE { Block($2) }
  | FN ID LPAREN params RPAREN COLON statement { FuncDecl($2, $4, $7) }

params:
  | { [] }
  | ID { [$1] }
  | ID COMMA params { $1 :: $3 }

statements:
  | statement { [$1] }
  | statement statements { $1 :: $2 }

expr:
  | INT { Int($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | ID { Id($1) }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { BinOp(Plus, $1, $3) }
  | expr MINUS expr { BinOp(Minus, $1, $3) }
  | expr TIMES expr { BinOp(Times, $1, $3) }
  | expr DIV expr { BinOp(Div, $1, $3) }
  | expr LT expr { BinOp(Lt, $1, $3) }
  | expr LEQ expr { BinOp(Leq, $1, $3) }
  | expr GT expr { BinOp(Gt, $1, $3) }
  | expr GEQ expr { BinOp(Geq, $1, $3) }
  | expr EQ expr { BinOp(Eq, $1, $3) }
  | expr NEQ expr { BinOp(Neq, $1, $3) }
  | expr AND expr { BinOp(And, $1, $3) }
  | expr OR expr { BinOp(Or, $1, $3) }
  | NOT expr { Not($2) }
