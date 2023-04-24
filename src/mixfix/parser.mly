%{
  open Input
%}

%token TINT TBOOL TLIST TARROW
%token <Input.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EQUAL LESS
%token IF THEN ELSE
%token FUN DARROW
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
%token EOF

%start toplevel file
%type <Input.toplevel_cmd list> file
%type <Input.toplevel_cmd> toplevel

%nonassoc IS
%right DARROW
%nonassoc ELSE
%nonassoc EQUAL LESS
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

def: LET VAR EQUAL expr { Def ($2, $4) }

basic:
  | LPAREN expr RPAREN		  { Expr $2 }
  | VAR		        	  { Var $1 }
  | INT		                  { Int $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | FUN VAR COLON ty DARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr { Rec ($2, $4, $6) }
  | MATCH expr WITH nil DARROW expr ALTERNATIVE VAR CONS VAR DARROW expr
      { Match ($2, $4, $6, $8, $10, $12) }

expr:
  | basic               { [$1] }
  | basic expr          { $1 :: $2 }

app:
    app non_app         { Apply ($1, $2) }
  | FST non_app         { Fst $2 }
  | SND non_app         { Snd $2 }
  | non_app non_app     { Apply ($1, $2) }

non_app:
  | nil                           { Nil $1 }
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
  | ty_times                 { $1 }
  | ty_times TARROW ty       { TArrow ($1, $3) }

ty_times :
  | ty_list                  { $1 }
  | ty_times TIMES ty_list   { TTimes ($1, $3) }


ty_list :
  | ty_simple { $1 }
  | ty_list TLIST            { TList $1 }

ty_simple :
  | TBOOL	 	     { TBool }
  | TINT         	     { TInt }
  | LPAREN ty RPAREN         { $2 }

%%
