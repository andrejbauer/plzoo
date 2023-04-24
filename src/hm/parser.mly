%{
open Syntax
%}

%token EOF SEMISEMI
%token YTOK
%token <string> IDENT
%token <int> INT
%token EQUAL PLUS
%token LPAREN RPAREN
%token LET IN
%token RIGHTARROW FUN
%token REF BANG COLONEQUAL

%nonassoc FUN
%left FUNAPP
%nonassoc INT IDENT LPAREN YTOK PLUS REF BANG COLONEQUAL

%start file
%type <Syntax.command list> file

%start toplevel
%type <Syntax.command> toplevel

%%
file: list(command) EOF { $1 }
toplevel: command SEMISEMI { $1 }

command:
  | LET name=name EQUAL e=expr { Def (name, e) }

expr:
  | e=simple_expr %prec FUN { e }
  | f=simple_expr l=list_expr %prec FUNAPP
     { App (f,List.rev l) }
  | LET name=name EQUAL e1=expr IN e2=expr { Let (name, e1, e2) }

simple_expr:
  | v=value { V v }
  | name=name { Var name }
  | LPAREN e=expr RPAREN { e }

list_expr:
  | simple_expr  { [$1] }
  | list_expr simple_expr { $2 :: $1 }

value:
  | FUN name=name RIGHTARROW e=expr { Lambda (name, e) }
  | c=constant { Constant c }
  | YTOK { Y }

constant:
  | i=INT { Int i }
  | PLUS { Plus }
  | REF { NewRef }
  | BANG { Get }
  | COLONEQUAL { Set }

name:
  | name=IDENT { Name.dummy name }
