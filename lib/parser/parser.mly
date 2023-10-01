%{
  open Ast
  open Ast_types
%}

%token  <int> INT
%token  <string> ID
// %token  <string> MUTEX_ID
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
%token ARROW
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
%token  CONST 
%token  VAR 
%token  STRUCT
%token  TRAIT
%token  IMPL
%token  NEW
%token  FUNCTION 
// %token  MUTEX
// // %token  MUTEX_ID
// %token  LOCK
// %token  UNLOCK
// %token  CREATE_THREAD
// %token  CONSUME
%token  FINISH 
%token  ASYNC 
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
%token PRINTF
%token <string> STRING
%token  MAIN
%token  EOF

%right  COLONEQ   EQUAL              
%left PLUS MINUS  LANGLE RANGLE
%left MULT DIV REM
%left AND OR  
%nonassoc EXCLAMATION_MARK

%start program 
%type <program> program
%type <struct_defn> struct_defn
%type <trait_defn> trait_defn
// %type <method_signature> method_signature
%type <impl_defn> impl_defn
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
    | struct_defns=list(struct_defn); 
    trait_defns=list(trait_defn);   
    impl_defns=list(impl_defn);
    function_defns=list(function_defn); 
    main=main_expr;  EOF 
    {Prog(struct_defns, trait_defns, impl_defns, function_defns, main)}

// Productions related to struct and interface definitions
struct_defn:
    | STRUCT; name=ID; 
    LBRACE; 
    capability=capability_defn; 
    field_defns=nonempty_list(field_defn); 
    RBRACE 
    { TStruct(Struct_name.of_string name, capability, field_defns)}

trait_defn: 
    | TRAIT; name=ID; 
    LBRACE; 
    method_signatures=list(method_signature);
    RBRACE
    { TTrait(Trait_name.of_string name, method_signatures)}

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

struct_capability_annotations:
| COLON;  capability_names=separated_nonempty_list(COMMA,capability_name){capability_names}

borrowed_ref:
    | BORROWED {Borrowed}

// Field definitions
modifier:
    | CONST {MConst}
    | VAR {MVar}

capability_name:
    | cap_name=ID {Capability_name.of_string cap_name}

field_defn:
    | m=modifier; field_type=type_expr; field_name=ID; capability_names=struct_capability_annotations SEMICOLON {TField(m, field_type, Field_name.of_string field_name, capability_names)}

// Method and function definitions
params:
    | LPAREN; params=separated_list(COMMA,param); RPAREN {params}

param_capability_annotations:
    | LBRACE;  capability_names=separated_nonempty_list(COMMA,capability_name); RBRACE {capability_names}

param:
    | maybeBorrowed=option(borrowed_ref); param_type=type_expr; capability_guards=option(param_capability_annotations); param_name=ID;  
    {Param(param_type, Var_name.of_string param_name, capability_guards, maybeBorrowed)}

impl_defn: 
    | IMPL trait_name=ID; FOR; struct_name=ID; method_defns=list(method_defn); {TImpl(Trait_name.of_string trait_name, Struct_name.of_string struct_name, method_defns)}

method_signature:
    |  method_name=ID;
    maybeBorrowed=option(borrowed_ref);  
    capabilities_used = struct_capability_annotations;
    method_params=params; ARROW;
    return_type=type_expr;
   { { name = Method_name.of_string method_name;
        borrowed = maybeBorrowed;
        capability = capabilities_used;
        params = method_params;
        return_type = return_type;
      } }

method_defn: 
    | LBRACE 
    method_signatures = method_signature;
    body=block_expr; RBRACE;
    {TMethod(method_signatures, body)}

function_signature:
    | function_name=ID; 
    maybeBorrowed=option(borrowed_ref); 
    function_params=params; ARROW;
    return_type=type_expr;
    { { name = Function_name.of_string function_name;
        borrowed = maybeBorrowed;
        return_type = return_type;
        params = function_params;
      } }

function_defn: 
    | FUNCTION; 
    function_signatures = function_signature;
    body=block_expr 
    {TFunction(function_signatures, body)}

// Types
type_expr : 
    | struct_name=ID {TEStruct(Struct_name.of_string struct_name)}
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

// Method / function arguments
args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN {args}

identifier:
    | variable=ID {Variable(Var_name.of_string variable)}
    | obj=ID DOT field=ID {ObjField(Var_name.of_string obj, Field_name.of_string field)}

constructor_args:
    | field_name=ID; COLON; assigned_expr=expr {ConstructorArg(Field_name.of_string field_name, assigned_expr)}

expr:
    | i=INT {{ loc=loc_of_position $startpos; node=Int(i) }}
    | TRUE {{ loc=loc_of_position $startpos; node=Boolean(true) }}
    | FALSE {{ loc=loc_of_position $startpos; node=Boolean(false) }}
    | LPAREN e=expr RPAREN {e}
    | id=identifier {{ loc=loc_of_position $startpos; node=Identifier(id) }}
    | op=un_op; e=expr {{ loc=loc_of_position $startpos; node=UnOp(op,e) }}
    | e1=expr; op=bin_op; e2=expr {{ loc=loc_of_position $startpos; node=BinOp(op, e1, e2) }}
    | NEW; var_name=ID; EQUAL; struct_name=ID; LBRACE constructor_args=separated_list(COMMA, constructor_args) RBRACE {{ loc=loc_of_position $startpos; node=Constructor(Var_name.of_string var_name, Struct_name.of_string struct_name, constructor_args) }}
    | LET; var_name=ID; type_annot=option(let_type_annot);  EQUAL; bound_expr=expr {{ loc=loc_of_position $startpos; node=Let(type_annot, Var_name.of_string var_name, bound_expr) }} 
    | id=identifier; COLONEQ; assigned_expr=expr {{ loc=loc_of_position $startpos; node=Assign(id, assigned_expr) }}
    | obj=ID; DOT; method_name=ID; method_args=args {{ loc=loc_of_position $startpos; node=MethodApp(Var_name.of_string obj, Method_name.of_string method_name, method_args) }}
    | fn=ID; fn_args=args {{ loc=loc_of_position $startpos; node=FunctionApp(Function_name.of_string fn, fn_args) }} 
    | IF; LPAREN cond_expr=expr; RPAREN then_expr=block_expr; ELSE; else_expr=block_expr {{ loc=loc_of_position $startpos; node=If(cond_expr, then_expr, else_expr) }}
    | WHILE; LPAREN; cond_expr=expr; RPAREN; loop_expr=block_expr {{ loc=loc_of_position $startpos; node=While(cond_expr, loop_expr) }}
    | FOR; LPAREN; init_expr=expr; SEMICOLON; cond_expr=expr; SEMICOLON; step_expr=expr; RPAREN; loop_expr=block_expr 
        {{ loc=loc_of_position $startpos; node=For(init_expr, cond_expr, step_expr, loop_expr) }}
    | PRINTF; LPAREN; format_str=STRING; option(COMMA); args=separated_list(COMMA, expr); RPAREN {{ loc=loc_of_position $startpos; node=Printf(format_str,args) }}
    | FINISH; LBRACE; forked_async_exprs=list(async_expr); curr_thread_expr=separated_list(SEMICOLON, expr) RBRACE {{ loc=loc_of_position $startpos; node=FinishAsync(forked_async_exprs, Block(loc_of_position $startpos, curr_thread_expr))}}

async_expr:
| ASYNC exprs=block_expr {AsyncExpr exprs}


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