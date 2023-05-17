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
%token  DOUBLECOLON
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
%token  NEW 
%token  CONST 
%token  VAR 
%token  STRUCT
%token  TYPE
%token  INTERFACE
%token  FUNCTION 
// %token  CONSUME
// %token  FINISH 
// %token  ASYNC 
// %token  CLASS
// %token  EXTENDS 
%token  GENERIC_TYPE 
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
%type <struct_definition> struct_defn
%type <function_definition> function_defn
%type <block_expr> main_expr
%type <block_expr> block_expr

%%

program: 
    | struct_defns=separated_list(SEMICOLON, struct_defn); interface_defns=separated_list(SEMICOLON, interface_defn); 
    function_defns=list(function_defn); main=main_expr;  EOF 
    {Prog(struct_defns, interface_defns, function_defns, main)}

// Productions related to struct and interface definitions
struct_defn:
    | TYPE; name=ID; maybe_generic=option(generic_type) STRUCT; LBRACE; capability=capability_defn; field_defns=nonempty_list(field_defn); RBRACE 
    { Struct(Struct_name.of_string name, maybe_generic, capability, field_defns)}

interface_defn: 
    | TYPE; name=ID; INTERFACE; LBRACE; method_signatures=nonempty_list(method_signature); RBRACE
    { Interface(Interface_name.of_string name, method_signatures)}

method_signature:
    | method_name=ID; method_params=params; DOUBLECOLON; return_type=type_expr;  SEMICOLON 
    {MethodSignature( Method_name.of_string method_name, method_params, return_type)}

generic_type:
    | LANGLE GENERIC_TYPE RANGLE  { Generic }

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
    | COLON;  capability_names=separated_nonempty_list(COMMA,capability_name){capability_names}

field_defn:
    | m=modifier; field_type=type_expr; field_name=ID; capability_names=struct_capability_annotations SEMICOLON {TField(m, field_type, Field_name.of_string field_name, capability_names)}

// Method and function definitions
params:
    | LPAREN; params=separated_list(COMMA,param); RPAREN {params}

param_capability_annotations:
    | LBRACE;  capability_names=separated_nonempty_list(COMMA,capability_name); RBRACE {capability_names}

param:
    | maybeBorrowed=option(borrowed_ref); param_type=type_expr; capability_guards=option(param_capability_annotations); param_name=ID;  {Param(param_type, Var_name.of_string param_name, capability_guards, maybeBorrowed)}

method_defn: 
    | maybeBorrowed=option(borrowed_ref); return_type=type_expr; method_name=ID; method_params=params; capabilities_used=struct_capability_annotations body=block_expr {Method( Method_name.of_string method_name, maybeBorrowed, return_type, method_params,capabilities_used,body)}

function_defn: 
    | FUNCTION; maybeBorrowed=option(borrowed_ref); return_type=type_expr; function_name=ID; function_params=params;  body=block_expr {Function(Function_name.of_string function_name, maybeBorrowed, return_type, function_params,body)}

// Types
parameterised_type:
    | LANGLE type_param=type_expr RANGLE {type_param}

type_expr : 
    | struct_name=ID maybe_param_type=option(parameterised_type) {TEStruct(Struct_name.of_string struct_name,maybe_param_type )}
    | TYPE_INT  {TEInt} 
    | TYPE_BOOL {TEBool}
    | TYPE_VOID {TEVoid}
    | GENERIC_TYPE {TEGeneric}

let_type_annot:
    | COLON ; type_annot=type_expr {type_annot}

// Expressions
main_expr:
    | TYPE_VOID; MAIN; LPAREN; RPAREN; exprs=block_expr {exprs}

block_expr:
    | LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE { Block(loc_of_position $startpos, exprs) }

// Method / function arguments
args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN {args}

constructor_arg:
    | field_name=ID; COLON; assigned_expr=expr {ConstructorArg(Field_name.of_string field_name, assigned_expr)}

identifier:
    | variable=ID {Variable(Var_name.of_string variable)}
    | obj=ID DOT field=ID {ObjField(Var_name.of_string obj, Field_name.of_string field)}

expr:
    | i=INT {{ loc=loc_of_position $startpos; node=Int(i) }}
    | TRUE {{ loc=loc_of_position $startpos; node=Boolean(true) }}
    | FALSE {{ loc=loc_of_position $startpos; node=Boolean(false) }}
    | LPAREN e=expr RPAREN {e}
    | id=identifier {{ loc=loc_of_position $startpos; node=Identifier(id) }}
    | op=un_op e=expr {{ loc=loc_of_position $startpos; node=UnOp(op,e) }}
    | e1=expr op=bin_op e2=expr {{ loc=loc_of_position $startpos; node=BinOp(op, e1, e2) }}
    | NEW; struct_name=ID; maybe_type_param=option(parameterised_type); LPAREN; constr_args=separated_list(COMMA, constructor_arg); RPAREN {{ loc=loc_of_position $startpos; node=Constructor(Struct_name.of_string struct_name, maybe_type_param, constr_args) }}
    | LET; var_name=ID; type_annot=option(let_type_annot);  EQUAL; bound_expr=expr  {{ loc=loc_of_position $startpos; node=Let(type_annot, Var_name.of_string var_name, bound_expr) }} 
    | id=identifier; COLONEQ; assigned_expr=expr {{ loc=loc_of_position $startpos; node=Assign(id, assigned_expr) }}
    // | CONSUME; id=identifier {Consume($startpos, id)}
    | obj=ID; DOT; method_name=ID; method_args=args {{ loc=loc_of_position $startpos; node=MethodApp(Var_name.of_string obj, Method_name.of_string method_name, method_args) }}
    | fn=ID; fn_args=args {{ loc=loc_of_position $startpos; node=FunctionApp(Function_name.of_string fn, fn_args) }} 
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