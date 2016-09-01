%{
  open Syntax
%}

%token TINT TBOOL TARROW
%token LBRACE RBRACE
%token COLON COMMA PERIOD
%token <Syntax.name> VAR
%token <int> INT
%token PLUS MINUS TIMES DIVIDE
%token TRUE FALSE
%token EQUAL LESS
%token AND OR NOT
%token IF THEN ELSE
%token FUN IS
%token LPAREN RPAREN
%token LET IN
%token SEMICOLON2
%token EOF

%start toplevel
%start file
%type <Syntax.toplevel_cmd> toplevel
%type <Syntax.toplevel_cmd list> file

%nonassoc IN
%right IS
%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIVIDE
%right TARROW

%%

file:
  | EOF                      { [] }
  | filedef                  { $1 }
  | fileexpr                 { $1 }

filedef:
  | def EOF                  { [$1] }
  | def SEMICOLON2 file      { $1 :: $3 }
  | def filedef              { $1 :: $2 }

fileexpr:
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 file     { Expr $1 :: $3 }

toplevel:
  | expr EOF                 { Expr $1 }
  | def EOF                  { $1 }

def:
  | LET VAR EQUAL expr { Def ($2, $4) }

expr:
  | non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | LET VAR EQUAL expr IN expr { Let ($2, $4, $6) }
  | IF expr THEN expr ELSE expr	       { If ($2, $4, $6) }
  | FUN VAR LPAREN VAR COLON ty RPAREN COLON ty IS expr { Fun ($2, $4, $6, $9, $11) }

app:
    app non_app         { App ($1, $2) }
  | non_app non_app     { App ($1, $2) }

non_app:
    VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | LPAREN expr RPAREN		  { $2 }    
  | LBRACE RBRACE               { Record [] }
  | LBRACE record_list RBRACE   { Record $2 }
  | non_app PERIOD VAR          { Project ($1, $3) }

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }
  | expr DIVIDE expr	{ Divide ($1, $3) }

boolean:
  | expr EQUAL expr     { Equal ($1, $3) }
  | expr LESS expr      { Less ($1, $3) }
  | expr AND expr       { And ($1, $3) }
  | expr OR expr        { Or ($1, $3) }
  | NOT expr         { Not $2 }

record_list:
  | field                   { [$1] }
  | field COMMA record_list { $1 :: $3 }

field:
  | VAR EQUAL expr          { ($1, $3) }

ty:
    TBOOL	 	     { TBool }
  | TINT         	     { TInt }
  | ty TARROW ty             { TArrow ($1, $3) }
  | LBRACE RBRACE            { TRecord [] }
  | LBRACE trecord_list RBRACE { TRecord $2 }
  | LPAREN ty RPAREN         { $2 }

trecord_list:
  | tfield                    { [$1] }
  | tfield COMMA trecord_list { $1 :: $3 }

tfield:
  | VAR COLON ty              { ($1, $3) }

%%
