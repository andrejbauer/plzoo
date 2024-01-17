%{
  open Syntax
%}

%token TINT TBOOL TLIST TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EQUALS LESS
%token IF THEN ELSE
%token FUN DARROW
%token COLON
%token STAR
%token LPAREN RPAREN
%token LET SET_EQUAL
%token SEMICOLON2
%token COMMA
%token FST
%token SND
%token LBRACK RBRACK
%token CONS
%token MATCH WITH ALTERNATIVE
%token REC IS
%token QUIT
%token EOF

%start toplevel file
%type <Syntax.toplevel_cmd list> file
%type <Syntax.toplevel_cmd> toplevel

%nonassoc IS
%right DARROW
%nonassoc ELSE
%nonassoc EQUALS LESS
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right CONS

%%

file:
  | EOF                      { [] }
  | lettop                   { $1 }
  | exprtop                  { $1 }
  | cmdtop                   { $1 }

lettop:
  | def EOF                  { [$1] }
  | def lettop               { $1 :: $2 }
  | def SEMICOLON2 file      { $1 :: $3 }

exprtop:
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 file     { Expr $1 :: $3 }

cmdtop:
  | cmd EOF                  { [$1] }
  | cmd SEMICOLON2 file      { $1 :: $3 }

toplevel:
  | def EOF    { $1 }
  | expr EOF   { Expr $1 }
  | cmd EOF    { $1 }

cmd:
  | QUIT       { Quit }

def: LET VAR SET_EQUAL expr { Def ($2, $4) }

expr:
  | non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | expr CONS expr      { Cons ($1, $3) }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR COLON ty DARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr { Rec ($2, $4, $6) }
  | MATCH expr WITH nil DARROW expr ALTERNATIVE VAR CONS VAR DARROW expr
      { Match ($2, $4, $6, $8, $10, $12) }

app: // application
    app non_app         { Apply ($1, $2) }
  | FST non_app         { Fst $2 }
  | SND non_app         { Snd $2 }
  | non_app non_app     { Apply ($1, $2) }

non_app: // non-application
    VAR		        	              { Var $1 }
  | TRUE                	        { Bool true }
  | FALSE               	        { Bool false }
  | INT		                        { Int $1 }
  | nil                           { Nil $1 }
  | LPAREN expr RPAREN		        { $2 }
  | LPAREN expr COMMA expr RPAREN { Pair ($2, $4) }

arith:
| MINUS INT                  { Int (-$2) }
| PLUS expr expr	           { Plus ($2, $3) }
| MINUS expr expr	           { Minus ($2, $3) }
| TIMES expr expr	           { Times ($2, $3) }
| DIVIDE expr expr	         { Divide ($2, $3) }
| MOD expr expr	             { Mod ($2, $3) }

nil: LBRACK ty RBRACK          { $2 }

boolean:
| EQUALS expr expr           { Equal ($2, $3) }
| LESS expr expr             { Less ($2, $3) }

ty:
  | ty_times                 { $1 }
  | ty_times TARROW ty       { TArrow ($1, $3) }

ty_times:
  | ty_list                  { $1 }
  | ty_times STAR ty_list   { TTimes ($1, $3) }

ty_list:
  | ty_simple { $1 }
  | ty_list TLIST            { TList $1 }

ty_simple:
  | TBOOL	 	     { TBool }
  | TINT         	     { TInt }
  | LPAREN ty RPAREN         { $2 }

%%
