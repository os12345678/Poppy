%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token <string> TYPE
%token <string> STRING
%token LET
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET
%token PLUS MINUS TIMES DIV
%token LT LEQ GT GEQ EQ NEQ
%token AND OR NOT XOR
%token IF ELSE
%token WHILE FOR
%token ASSIGN
%token COLON SEMICOLON
%token FN 
%token RETURN
%token PRINT
%token COMMA
%token EOF

%type <Ast.expr> expr
%type <Ast.statement> statement
%type <Ast.statement list> statements
%type <Ast.expr list> args
%type <Ast.expr list> elements
%type <Ast.expr> increment
%type <Ast.func_param list> params

%nonassoc EQ NEQ
%nonassoc LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV
%left AND
%left OR XOR
%right NOT

%start main
%type <Ast.statement list> main
%%

main:
  | statements EOF { $1 }

statement:
  | LET ID ASSIGN expr SEMICOLON { Let($2, $4) }
  | ID ASSIGN expr SEMICOLON { Assign($1, $3) }
  | IF LPAREN expr RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE { If($3, Block($6), Block($10)) }
  | WHILE LPAREN expr RPAREN LBRACE statements RBRACE { While($3, Block($6)) }
  | FOR LPAREN ID ASSIGN INT COMMA expr COMMA increment RPAREN LBRACE statements RBRACE { For($3, $5, $7, $9, Block($12)) }
  | LBRACE statements RBRACE { Block($2) }
  | FN ID LPAREN params RPAREN LBRACE statements RBRACE { FuncDecl(Id $2, $4, $7) }
  | RETURN expr SEMICOLON { Return($2) }

expr_statement:
  | expr SEMICOLON { Expr($1) }

params: (* func_param type*)
  | { [] }
  | ID COLON TYPE { [Param (Id $1, Type $3)] }
  | ID COLON TYPE COMMA params { Param(Id $1, Type $3) :: $5 }

increment:
  | ID { Id(Id($1)) }
  | ID PLUS PLUS { Incr($1) }
  | ID MINUS MINUS { Decr($1) }

statements:
  | statement statements { $1 :: $2 }
  | expr_statement statements { $1 :: $2 }
  | { [] }

expr:
  | INT { Int($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | ID { Id(Id($1)) }
  | TYPE { Type(Type($1)) }
  | STRING { StringLiteral($1) }
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
  | expr XOR expr { BinOp(Xor, $1, $3) }
  | NOT expr { Not($2) }
  | PRINT LPAREN format_str=STRING RPAREN { Print(format_str) }
  | LBRACKET elements RBRACKET { List($2) }
  | ID LPAREN args RPAREN { Builtin($1, $3) }

elements:
  | expr { [$1] }
  | expr COMMA elements { $1 :: $3 }

args:
  | { [] }
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }
