%{
  (* header *)
  open Syntax

  let fold_lams lst e = List.fold_right (fun x e -> Lam (x, e)) lst e

  let parse_error _ =
    raise (Message.Parse (Message.loc_here 1, "parse error"))

%}

/* Tokens */

%token COMMA
%token EOF
%token EQUAL
%token FST
%token <string> IDENT
%token LAMBDA
%token LPAREN
%token PERIOD
%token RPAREN
%token SEMICOLON
%token SND

/* Precedence and associativity */

%nonassoc SEMICOLON
%nonassoc EQUAL
%nonassoc FST SND
%nonassoc LAMBDA PERIOD

/* Entry points */

%start shell script
%type <Syntax.toplevel option> shell
%type <Syntax.toplevel list> script

%%

script:
  | command SEMICOLON script     { $1 :: $3 }
  | EOF                          { [] }

shell:
  | command EOF { Some $1 }
  | EOF         { None }

command:
  | IDENT EQUAL expr           { Definition ($1, $3) }
  | expr                       { Expr $1 }

expr:
  | non_lam                    { $1 }
  | LAMBDA idents PERIOD expr  { fold_lams $2 $4 }

idents:
  | IDENT                      { [$1] }
  | IDENT idents               { $1 :: $2 }

simple:
  | IDENT                      { Id $1 }
  | LPAREN expr COMMA expr RPAREN { Pair ($2, $4) }
  | FST expr                   { Fst $2 }
  | SND expr                   { Snd $2 }
  | LPAREN expr RPAREN         { $2 }

non_lam:
  | non_lam simple             { App ($1, $2) }
  | simple                     { $1 }
