%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TFORGET
%token TFREE
%token TARROW
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
%token SEMICOLON2
%token RETURN THUNK FORCE
%token QUIT
%token USE
%token <string>STRING
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc TO PERIOD
%nonassoc LET IN
%nonassoc FUN ARROW REC IS
%nonassoc IF THEN ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%right TARROW

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
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | LET VAR EQUAL expr IN expr  { Let ($2, $4, $6) }
  | expr TO VAR IN expr         { To ($1, $3, $5) }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR COLON ty ARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr    { Rec ($2, $4, $6) }
  
app:
  | non_app            { $1 }
  | FORCE non_app      { Force $2 }
  | RETURN non_app     { Return $2 }
  | THUNK non_app      { Thunk $2 }
  | app non_app        { Apply ($1, $2) }

non_app:
  | VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | LPAREN expr RPAREN		  { $2 }    

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }

boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }

ty:
  | TINT         	     { VInt }
  | TBOOL	 	     { VBool }
  | ty ARROW ty              { CArrow ($1, $3) }
  | TFORGET ty               { VForget $2 }
  | TFREE ty                 { CFree $2 }
  | LPAREN ty RPAREN         { $2 }

%%
