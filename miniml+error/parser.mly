%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token COLON
%token LPAREN RPAREN
%token LET
%token SEMICOLON2
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc FUN IS
%nonassoc IF THEN ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIVIDE
%left COLON
%right TARROW

%%

toplevel:
    EOF                      { [] }
  | def EOF                  { [$1] }
  | def SEMICOLON2 EOF       { [$1] }
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 EOF      { [Expr $1] }
  | def SEMICOLON2 toplevel  { $1 :: $3 }
  | expr SEMICOLON2 toplevel { (Expr $1) :: $3 }

def: LET VAR EQUAL expr { Def ($2, $4) }

expr:
    non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR LPAREN VAR COLON ty RPAREN COLON ty IS expr { Fun ($2, $4, $6, $9, $11) }

app:
    app non_app         { Apply ($1, $2) }
  | non_app non_app     { Apply ($1, $2) }

non_app:
    VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | LPAREN expr RPAREN		  { $2 }    

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }
  | expr DIVIDE expr	{ Division ($1, $3) }

boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }

ty:
    TBOOL	 { TBool }
  | TINT         { TInt }
  | ty TARROW ty { TArrow ($1, $3) }
  | LPAREN ty RPAREN { $2 }

%%

