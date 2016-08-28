%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TFORGET
%token TFREE
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN ARROW
%token REC IS
%token COLON
%token LPAREN RPAREN
%token LET IN
%token TO
%token SEMISEMI
%token RETURN THUNK FORCE
%token EOF

%start toplevel
%start file
%type <Syntax.toplevel> toplevel
%type <Syntax.toplevel list> file

%nonassoc TO
%nonassoc IN
%nonassoc IS
%right ARROW
%right TFREE TFORGET
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

toplevel:
  | d = def SEMISEMI EOF
    { d }
  | e = expr SEMISEMI EOF
    { Expr e }

def:
  | LET VAR EQUAL expr
    { Def ($2, $4) }

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | plain_app                 { $1 }
  | plain_arith               { $1 }
  | plain_boolean             { $1 }
  | LET VAR EQUAL expr IN expr  { Let ($2, $4, $6) }
  | expr TO VAR IN expr         { To ($1, $3, $5) }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR COLON ty ARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr    { Rec ($2, $4, $6) }
  
app: mark_position(plain_app) { $1 }
plain_app:
  | plain_non_app      { $1 }
  | FORCE non_app      { Force $2 }
  | RETURN non_app     { Return $2 }
  | THUNK non_app      { Thunk $2 }
  | app non_app        { Apply ($1, $2) }

non_app: mark_position(plain_non_app) { $1 }
plain_non_app:
  | VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | LPAREN plain_expr RPAREN		  { $2 }    

arith: mark_position(plain_arith) { $1 }
plain_arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }

boolean: mark_position(plain_boolean) { $1 }
plain_boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }

ty:
  | TINT         	     { VInt }
  | TBOOL	 	           { VBool }
  | ty ARROW ty        { CArrow ($1, $3) }
  | TFORGET ty         { VForget $2 }
  | TFREE ty           { CFree $2 }
  | LPAREN ty RPAREN   { $2 }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%
