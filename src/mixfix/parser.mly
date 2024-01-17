%{
  open Presyntax
  open Syntax
%}

%token TINT TBOOL TLIST TARROW
%token <Presyntax.name> VAR
%token <int> INT
%token <string> MIX_DEF
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EQUALS LESS
%token COND // Prefix IF THEN ELSE
%token FUN DARROW
%token COLON
%token STAR
%token LPAREN RPAREN
%token LET SET_EQUAL
%token SEMICOLON2
%token PAIR
%token FST
%token SND
%token LBRACK RBRACK
%token CONS
%token MATCH WITH ALTERNATIVE
%token REC IS
%token QUIT
%token EOF

%start toplevel file
%type <Presyntax.toplevel_cmd list> file
%type <Presyntax.toplevel_cmd> toplevel

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

def: 
  | LET VAR SET_EQUAL expr  { Def ($2, $4) }
  | MIX_DEF VAR INT expr    { MixDef ($1, $2, $3, $4) }

expr:
  | seq                             { $1 }
  | FUN VAR COLON ty DARROW expr    { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr        { Rec ($2, $4, $6) }
  | MATCH expr WITH nil DARROW expr ALTERNATIVE VAR CONS VAR DARROW expr
      { Match ($2, $4, $6, $8, $10, $12) }

seq:
  | app+            { Seq $1 }

app: // application
  | non_app             { $1 }

  | FST non_app         { Fst $2 }
  | SND non_app         { Snd $2 }

  | PLUS  non_app  non_app                { Plus ($2, $3) }
  | MINUS  non_app  non_app               { Minus ($2, $3) }
  | TIMES  non_app  non_app               { Times ($2, $3) }
  | DIVIDE  non_app  non_app              { Divide ($2, $3) }
  | MOD  non_app  non_app                 { Mod ($2, $3) }

  | EQUALS non_app non_app                { Equal ($2, $3) }
  | LESS non_app non_app                  { Less ($2, $3) }

  | COND non_app non_app non_app          { If ($2, $3, $4) }
  | CONS non_app non_app                  { Cons ($2, $3) }
  | PAIR non_app non_app                  { Pair ($2, $3) }

non_app: // non-application
    VAR                           { Var $1 }
  | TRUE                          { Bool true }
  | FALSE                         { Bool false }
  | INT                           { Int $1 }
  | nil                           { Nil $1 }
  | LPAREN expr RPAREN            { $2 }

nil: LBRACK ty RBRACK          { $2 }

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
  | TBOOL                    { TBool }
  | TINT                     { TInt }
  | LPAREN ty RPAREN         { $2 }

%%
