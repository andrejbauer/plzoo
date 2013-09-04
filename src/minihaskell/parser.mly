%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TTIMES
%token TARROW
%token TLIST
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EQUAL LESS
%token IF THEN ELSE
%token FUN ARROW
%token COLON
%token LPAREN RPAREN
%token LET
%token SEMICOLON2
%token COMMA
%token FST
%token SND
%token LBRACK RBRACK
%token CONS
%token MATCH WITH ALTERNATIVE
%token REC IS
%token QUIT
%token USE
%token <string>STRING
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc REC IS
%right FUN ARROW
%nonassoc MATCH WITH
%nonassoc IF THEN ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right CONS
%right TARROW
%left TTIMES
%nonassoc TLIST

%%

toplevel:
  | EOF                      { [] }
  | lettop                   { $1 }
  | exprtop                  { $1 }
  | cmdtop                   { $1 }

lettop:
  | def EOF                  { [$1] }
  | def lettop               { $1 :: $2 }
  | def SEMICOLON2 toplevel  { $1 :: $3 }

exprtop:
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 toplevel { Expr $1 :: $3 }

cmdtop:
  | cmd EOF                  { [$1] }
  | cmd SEMICOLON2 toplevel  { $1 :: $3 }

cmd:
  | USE STRING { Use $2 }
  | QUIT       { Quit }

def: LET VAR EQUAL expr { Def ($2, $4) }

expr:
  | non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | expr CONS expr      { Cons ($1, $3) }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR COLON ty ARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr { Rec ($2, $4, $6) }
  | MATCH expr WITH nil ARROW expr ALTERNATIVE VAR CONS VAR ARROW expr
      { Match ($2, $4, $6, $8, $10, $12) }

app:
    app non_app         { Apply ($1, $2) }
  | FST non_app         { Fst $2 }
  | SND non_app         { Snd $2 }
  | non_app non_app     { Apply ($1, $2) }

non_app:
    VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | nil                           { Nil $1 }
  | LPAREN expr RPAREN		  { $2 }    
  | LPAREN expr COMMA expr RPAREN { Pair ($2, $4) }

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }
  | expr DIVIDE expr	{ Divide ($1, $3) }
  | expr MOD expr	{ Mod ($1, $3) }

nil: LBRACK ty RBRACK   { $2 }

boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }

ty:
    TBOOL	 	     { TBool }
  | TINT         	     { TInt }
  | ty TIMES ty { TTimes ($1, $3) }
  | ty ARROW ty { TArrow ($1, $3) }
  | ty TLIST                 { TList $1 }
  | LPAREN ty RPAREN         { $2 }

%%
