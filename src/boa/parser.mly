%{
  open Syntax
%}

%token LBRACE RBRACE WITH COPY
%token COMMA SEMICOLON PERIOD
%token <Syntax.name> VAR
%token THIS
%token <int> INT
%token PLUS MINUS TIMES DIVIDE REMAINDER
%token TRUE FALSE
%token EQUAL UNEQUAL LESS
%token AND OR NOT
%token IF THEN ELSE
%token FUN ARROW
%token LPAREN RPAREN
%token LET IN
%token ASSIGN SKIP
%token SEMICOLON2
%token EOF

%start toplevel file
%type <Syntax.toplevel_cmd> toplevel
%type <Syntax.toplevel_cmd list> file

%nonassoc IN
%right ARROW
%right SEMICOLON
%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL UNEQUAL LESS
%nonassoc ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE REMAINDER
%nonassoc COPY
%left WITH
%left PERIOD


%%

file:
  | EOF                      { [] }
  | command SEMICOLON2 file  { $1 :: $3 }
  | command EOF              { [$1] }

toplevel:
  | command SEMICOLON2? EOF  { $1 }

command:
  | expr                 { Expr $1 }
  | LET VAR EQUAL expr   { Def ($2, $4) }

expr:
  | non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | IF expr THEN expr ELSE expr	       { If ($2, $4, $6) }
  | FUN VAR ARROW expr                 { Fun ($2, $4) }
  | LET VAR EQUAL expr IN expr         { Let ($2, $4, $6) }
  | non_app PERIOD VAR ASSIGN expr     { Assign ($1, $3, $5) }
  | expr SEMICOLON expr                { Seq ($1, $3) }

app:
    app non_app         { App ($1, $2) }
  | non_app non_app     { App ($1, $2) }

non_app:
    VAR		        	  { Var $1 }
  | THIS                          { This }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | SKIP                          { Skip }
  | LPAREN expr RPAREN		  { $2 }    
  | non_app PERIOD VAR            { Project ($1, $3) }
  | LBRACE fields RBRACE          { Object $2 }
  | COPY non_app                  { Copy $2 }
  | non_app WITH non_app          { With ($1, $3) }

arith:
  | expr PLUS   expr	{ ArithOp (Plus, $1, $3) }
  | expr MINUS expr	{ ArithOp (Minus, $1, $3) }
  | expr TIMES  expr	{ ArithOp (Times, $1, $3) }
  | expr DIVIDE expr	{ ArithOp (Divide, $1, $3) }
  | expr REMAINDER expr	{ ArithOp (Remainder, $1, $3) }

boolean:
  | NOT expr            { Not $2 }
  | expr LESS expr      { CmpOp (Less, $1, $3) }
  | expr EQUAL expr     { CmpOp (Equal, $1, $3) }
  | expr UNEQUAL expr   { CmpOp (Unequal, $1, $3) }
  | expr AND expr       { BoolOp (And, $1, $3) }
  | expr OR expr        { BoolOp (Or, $1, $3) }

field:
  | VAR EQUAL expr              { ($1, $3) }

fields:
  |                                 { [] }
  | field                           { [$1] }
  | field COMMA fields              { $1 :: $3 }

%%
