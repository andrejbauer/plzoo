%{
  open Syntax
%}

%token COPY AS
%token BOX
%token <Syntax.name> VAR
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token IF THEN ELSE
%token TYINT TYBOOL TYSTRING
%token LET EQUAL IN
%token LPAREN RPAREN
%token COMMA SEMICOLON
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc SEMICOLON
%nonassoc BOX
%nonassoc LET IN COPY AS

%%

toplevel:
   | EOF                      { [] }
   | expr EOF                 { [Expr $1 ] }
   | def EOF                  { [Def $1] }
   | expr SEMICOLON toplevel  { Expr $1 :: $3 }
   | def SEMICOLON toplevel   { Def $1 :: $3 }

def: BOX VAR LPAREN vars RPAREN EQUAL expr { ($2, $4, $7) }

vars:
  |                      { [] }
  | ty VAR               { [($1, $2)] }
  | ty VAR COMMA vars    { ($1,$2) :: $4 }

ty:
  | TYINT                { TyInt }
  | TYBOOL               { TyBool }
  | TYSTRING             { TyString }

expr:
  | simple_expr                                      { $1 }
  | COPY simple_expr AS VAR COMMA VAR IN expr        { Copy ($2, $4, $6, $8) }
  | LET VAR EQUAL simple_expr IN expr                { Let ($2, $4, $6) }
  | IF simple_expr THEN simple_expr ELSE simple_expr { Cond ($2, $4, $6) }

simple_expr:
  | VAR		        	             { Var $1 }
  | INT		                       { Int $1 }
  | BOOL	                       { Bool $1 }
  | STRING	                     { String $1 }
  | VAR LPAREN args RPAREN       { Apply ($1, $3) }
  | LPAREN expr RPAREN		       { $2 }    

args:
  |                            { [] }
  | simple_expr                { [$1] }
  | simple_expr COMMA args     { $1 :: $3 }

%%

