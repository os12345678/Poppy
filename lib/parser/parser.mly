%{
  open Ast
  open Ast_types
%}

%token  <int> INT
%token  <string> ID
%token  LPAREN
%token  RPAREN 
%token  LBRACE 
%token  RBRACE 
%token  LANGLE
%token  RANGLE
%token  COMMA 
%token  DOT 
%token  COLON 
// %token  DOUBLECOLON
%token  SEMICOLON 
%token  EQUAL 
%token  PLUS
%token  MINUS
%token  MULT
%token  DIV
%token  REM
%token  AND
%token  OR
%token  EXCLAMATION_MARK
%token  COLONEQ
%token  LET 
%token  TYPE 
%token  CONST 
%token  VAR 
%token  STRUCT
%token  INTERFACE
%token  FUNCTION 
// %token  CONSUME
// %token  FINISH 
// %token  ASYNC 
%token  CAPABILITY 
%token  LINEAR 
%token  LOCAL 
%token  READ 
%token  SUBORDINATE 
%token  LOCKED 
%token  TYPE_INT 
%token  TYPE_BOOL
%token  TYPE_VOID
%token  BORROWED
%token  TRUE
%token  FALSE
%token  IF
%token  ELSE
%token  FOR
%token  WHILE
%token  MAIN
%token EOF

%right  COLONEQ   EQUAL              
%left PLUS MINUS  LANGLE RANGLE
%left MULT DIV REM
%left AND OR  
%nonassoc EXCLAMATION_MARK

%start program 
%type <program> program
%type <struct_defn> struct_defn
%type <interface_defn> interface_defn
// %type <method_signature> method_signature
%type <method_defn> method_defn
%type <mode> mode
%type <capability> capability
%type <borrowed_ref> borrowed_ref
%type <modifier> modifier
%type <Capability_name.t> capability_name
%type <Capability_name.t list> struct_capability_annotations
%type <Capability_name.t list> param_capability_annotations
%type <field_defn> field_defn
%type <param list> params
%type <param> param
%type <function_defn> function_defn
%type <type_expr> type_expr
%type <type_expr> let_type_annot
%type <interface_method_defn> interface_method_defn


%type <block_expr> main_expr
%type <block_expr> block_expr
%type <expr list> args
%type <identifier> identifier
%type <expr> expr
// %type <async_expr> async_expr

%type <un_op> un_op
%type <bin_op> bin_op

%%

program: 
    | struct_defns=list(struct_defn); interface_defns=list(interface_defn);   
    function_defns=list(function_defn); main=main_expr;  EOF 
    {Prog(struct_defns, interface_defns, function_defns, main)}

// Productions related to struct and interface definitions
struct_defn:
    | STRUCT; name=ID; LBRACE; capability=capability_defn; field_defns=nonempty_list(field_defn); method_defns=nonempty_list(method_defn); RBRACE 
    { TStruct(Struct_name.of_string name, capability, field_defns, method_defns)}

interface_defn: 
    | INTERFACE; name=ID; LBRACE; method_defns=list(interface_method_defn); RBRACE
    { TInterface(Interface_name.of_string name, method_defns)}

// Modes and capabilities
mode:
    | LINEAR { Linear }
    | LOCAL { ThreadLocal }
    | READ  { Read }
    | SUBORDINATE { Subordinate }
    | LOCKED { Locked }

capability_defn:
    | CAPABILITY; capabilities=separated_nonempty_list(COMMA,capability); SEMICOLON; {capabilities}
    | {[]}

capability:
    | mode=mode; cap_name=ID {TCapability(mode, Capability_name.of_string cap_name)}

borrowed_ref:
    | BORROWED {Borrowed}

// Field definitions
modifier:
    | CONST {MConst}
    | VAR {MVar}

capability_name:
    | cap_name=ID {Capability_name.of_string cap_name}

struct_capability_annotations:
    | COLON; capability_names=separated_nonempty_list(COMMA,capability_name){capability_names}

field_defn:
    | m=modifier; field_type=type_expr; field_name=ID; capability_names=struct_capability_annotations SEMICOLON {TField(m, field_type, Field_name.of_string field_name, capability_names)}

// Method and function definitions
params:
    | LPAREN; params=separated_list(COMMA,param); RPAREN {params}

param_capability_annotations:
    | LBRACE;  capability_names=separated_nonempty_list(COMMA,capability_name); RBRACE {capability_names}

param:
    | maybeBorrowed=option(borrowed_ref); param_type=type_expr; capability_guards=option(param_capability_annotations); param_name=ID;  {Param(param_type, Var_name.of_string param_name, capability_guards, maybeBorrowed)}

interface_method_defn:
    | method_name=ID; maybeBorrowed=option(borrowed_ref); return_type=type_expr; method_params=params SEMICOLON
    { TInterfaceMethod(Method_name.of_string method_name, maybeBorrowed, return_type, method_params)}

method_defn: 
    | method_name=ID; maybeBorrowed=option(borrowed_ref); return_type=type_expr; method_params=params; body=block_expr {TMethod(Method_name.of_string method_name, maybeBorrowed, return_type, method_params, body)}

function_defn: 
    | FUNCTION; maybeBorrowed=option(borrowed_ref); return_type=type_expr; function_name=ID; function_params=params;  body=block_expr {TFunction(Function_name.of_string function_name, maybeBorrowed, return_type, function_params,body)}

// Types
type_expr : 
    | STRUCT struct_name=ID {TEStruct(Struct_name.of_string struct_name)}
    | INTERFACE interface_name=ID {TEInterface(Interface_name.of_string interface_name)}
    | TYPE_INT  {TEInt} 
    | TYPE_BOOL {TEBool}
    | TYPE_VOID {TEVoid}

let_type_annot:
    | COLON ; type_annot=type_expr {type_annot}

// Expressions
main_expr:
    | TYPE_VOID; MAIN; LPAREN; RPAREN; exprs=block_expr {exprs}

block_expr:
    | LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE { Block(loc_of_position $startpos, exprs) }

// TODO: following 3 productions are not working. 
interface_expr:
    | LET; var_name=ID; COLONEQ; struct_exprs=struct_expr 
        {{ loc=loc_of_position $startpos; node=AssignToInterface(Var_name.of_string var_name, struct_exprs) }} 

struct_expr:
    | TYPE struct_name=ID; LBRACE; field_inits=separated_list(COMMA, struct_field_init); RBRACE; 
        {{ loc=loc_of_position $startpos; node=NewStruct(Struct_name.of_string struct_name, field_inits) }} 

struct_field_init:
    | field_name=ID; EQUAL; exprs=expr
        { (Field_name.of_string field_name, exprs) }

// Method / function arguments
args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN {args}

identifier:
    | variable=ID {Variable(Var_name.of_string variable)}

expr:
    | struct_exprs=struct_expr { struct_exprs }
    | interface_exprs=interface_expr { interface_exprs }
    | i=INT {{ loc=loc_of_position $startpos; node=Int(i) }}
    | TRUE {{ loc=loc_of_position $startpos; node=Boolean(true) }}
    | FALSE {{ loc=loc_of_position $startpos; node=Boolean(false) }}
    | LPAREN e=expr RPAREN {e}
    | id=identifier {{ loc=loc_of_position $startpos; node=Identifier(id) }}
    | op=un_op; e=expr {{ loc=loc_of_position $startpos; node=UnOp(op,e) }}
    | e1=expr; op=bin_op; e2=expr {{ loc=loc_of_position $startpos; node=BinOp(op, e1, e2) }}
    | LET; var_name=ID; type_annot=option(let_type_annot);  EQUAL; bound_expr=expr  {{ loc=loc_of_position $startpos; node=Let(type_annot, Var_name.of_string var_name, bound_expr) }} 
    | id=identifier; COLONEQ; assigned_expr=expr {{ loc=loc_of_position $startpos; node=Assign(id, assigned_expr) }}
    | obj=ID; DOT; method_name=ID; method_args=args SEMICOLON{{ loc=loc_of_position $startpos; node=MethodApp(Var_name.of_string obj, Method_name.of_string method_name, method_args) }}
    | fn=ID; fn_args=args SEMICOLON{{ loc=loc_of_position $startpos; node=FunctionApp(Function_name.of_string fn, fn_args) }} 
    | IF; cond_expr=expr; then_expr=block_expr; ELSE; else_expr=block_expr {{ loc=loc_of_position $startpos; node=If(cond_expr, then_expr, else_expr) }}
    | WHILE cond_expr=expr; loop_expr=block_expr {{ loc=loc_of_position $startpos; node=While(cond_expr, loop_expr) }}
    | FOR; LPAREN; init_expr=expr; SEMICOLON; cond_expr=expr; SEMICOLON; step_expr=expr; RPAREN; loop_expr=block_expr 
        {{ loc=loc_of_position $startpos; node=For(init_expr, cond_expr, step_expr, loop_expr) }}

%inline un_op:
    | EXCLAMATION_MARK {UnOpNot}
    | MINUS {UnOpNeg}

%inline bin_op:
    | PLUS { BinOpPlus }
    | MINUS { BinOpMinus }
    | MULT { BinOpMult }
    | DIV { BinOpIntDiv } 
    | REM { BinOpRem }
    | LANGLE { BinOpLessThan }
    | LANGLE EQUAL { BinOpLessThanEq }
    | RANGLE { BinOpGreaterThan }
    | RANGLE EQUAL{ BinOpGreaterThanEq }
    | AND {BinOpAnd}
    | OR {BinOpOr}
    | EQUAL EQUAL {BinOpEq}
    | EXCLAMATION_MARK EQUAL {BinOpNotEq}