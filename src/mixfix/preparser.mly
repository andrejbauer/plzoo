%{
  open Presyntax
  
  let split_to_functions (a:(name * Syntax.htype) list) (e:expr) =
        List.fold_right (fun (x, ty) e -> Fun (x, ty, e)) a e

  let type_all_names (a:string list) (b:Syntax.htype) =
        List.map (fun x -> (x, b)) a
  
%}
%token TINT TBOOL TLIST TARROW
%token <Presyntax.name> VAR
%token <int> INT
%token MIXFIX LEFT RIGHT
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
  | EOF
    { [] }
  | topentry SEMICOLON2 file
    { $1 :: $3 }

topentry:
  | def
    { $1 }
  | expr
    { Expr $1 }
  | mixfix
    { $1 }
  | cmd
    { $1 }

toplevel:
  | topentry EOF
    { $1 }

cmd:
  | QUIT
    { Quit }

def:
  | LET VAR SET_EQUAL expr
    { Def ($2, $4) }

mixfix:
  | MIXFIX fixity INT VAR
    { Mixfix ($2, $3, $4) }

fixity:
  | LEFT
    { LeftAssoc }
  | RIGHT
    { RightAssoc }
  |
    { NonAssoc }

vars:
  | VAR
    { [$1] }
  | VAR vars
    { $1 :: $2 } 

typed_vars:
  | LPAREN vars COLON ty RPAREN
    { type_all_names $2 $4 }

funvardef:
  | vars COLON ty
    { type_all_names $1 $3 }
  | typed_vars+
    { List.flatten $1 }

expr:
  | seq
    { $1 }
  | FUN funvardef DARROW expr
    { split_to_functions $2 $4 }
  | REC VAR COLON ty IS expr
    { Rec ($2, $4, $6) }
  | MATCH expr WITH nil DARROW expr ALTERNATIVE VAR CONS VAR DARROW expr
    { Match ($2, $4, $6, $8, $10, $12) }

seq:
  | app+
    { Seq $1 }

app: // application
  | non_app
    { $1 }
  | FST non_app
    { Fst $2 }
  | SND non_app
    { Snd $2 }
  | PLUS non_app non_app
    { Plus ($2, $3) }
  | MINUS non_app non_app
    { Minus ($2, $3) }
  | TIMES non_app non_app
    { Times ($2, $3) }
  | DIVIDE non_app non_app
    { Divide ($2, $3) }
  | MOD non_app non_app
    { Mod ($2, $3) }
  | EQUALS non_app non_app
    { Equal ($2, $3) }
  | LESS non_app non_app
    { Less ($2, $3) }
  | COND non_app non_app non_app
    { If ($2, $3, $4) }
  | CONS non_app non_app
    { Cons ($2, $3) }
  | PAIR non_app non_app
    { Pair ($2, $3) }

non_app: // non-application
    VAR
    { Var $1 }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | INT
    { Int $1 }
  | nil
    { Nil $1 }
  | LPAREN expr RPAREN
    { $2 }

nil:
  | LBRACK ty RBRACK
    { $2 }
ty:
  | ty_times
    { $1 }
  | ty_times TARROW ty
    { Syntax.TArrow ($1, $3) }

ty_times:
  | ty_list
    { $1 }
  | ty_times STAR ty_list
    { Syntax.TTimes ($1, $3) }

ty_list:
  | ty_simple
    { $1 }
  | ty_list TLIST
    { Syntax.TList $1 }

ty_simple:
  | TBOOL
    { Syntax.TBool }
  | TINT
    { Syntax.TInt }
  | LPAREN ty RPAREN
    { $2 }
%%
