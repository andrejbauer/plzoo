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
%token FUN DARROW
%token REC IS
%token COLON
%token LPAREN RPAREN
%token DO ASSIGN
%token LET IN
%token SEMISEMI
%token RETURN THUNK FORCE
%token EOF

%start toplevel
%start file
%type <Syntax.toplevel> toplevel
%type <Syntax.toplevel list> file

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

toplevel:
  | d = def EOF
    { d }
  | e = expr EOF
    { Expr e }

def:
  | LET VAR EQUAL expr
    { Def ($2, $4) }

expr: mark_position(plain_expr)  { $1 }
plain_expr:
  | plain_boolean                { $1 }
  | LET VAR EQUAL expr IN expr   { Let ($2, $4, $6) }
  | DO VAR ASSIGN expr IN expr   { Do ($2, $4, $6) }
  | IF expr THEN expr ELSE expr  { If ($2, $4, $6) }
  | FUN VAR COLON ty DARROW expr { Fun ($2, $4, $6) }
  | REC VAR COLON ty IS expr     { Rec ($2, $4, $6) }
  
(* boolean: mark_position(plain_boolean) { $1 } *)
plain_boolean:
  | plain_arith         { $1 }
  | arith EQUAL arith   { Equal ($1, $3) }
  | arith LESS arith    { Less ($1, $3) }

arith: mark_position(plain_arith) { $1 }
plain_arith:
  | plain_factor        { $1 }
  | arith PLUS factor   { Plus ($1, $3) }
  | arith MINUS factor  { Minus ($1, $3) }

factor: mark_position(plain_factor) { $1 }
plain_factor:
  | plain_app           { $1 }
  | factor TIMES app    { Times ($1, $3) }

app: mark_position(plain_app) { $1 }
plain_app:
  | plain_simple      { $1 }
  | FORCE simple      { Force $2 }
  | RETURN simple     { Return $2 }
  | THUNK simple      { Thunk $2 }
  | app simple        { Apply ($1, $2) }

simple: mark_position(plain_simple) { $1 }
plain_simple:
  | VAR                       { Var $1 }
  | INT                       { Int $1 }
  | TRUE                      { Bool true }
  | FALSE                     { Bool false }
  | LPAREN plain_expr RPAREN  { $2 }    

ty: mark_position(plain_ty)   { $1 }
plain_ty:
  | plain_app_ty              { $1 }
  | app_ty TARROW ty          { CArrow ($1, $3) }

app_ty: mark_position(plain_app_ty) { $1 }
plain_app_ty:
  | plain_simple_ty           { $1 }
  | TFORGET simple_ty         { VForget $2 }
  | TFREE simple_ty           { CFree $2 }

simple_ty: mark_position(plain_simple_ty) { $1 }
plain_simple_ty:
  | TINT                     { VInt }
  | TBOOL                    { VBool }
  | LPAREN plain_ty RPAREN   { $2 }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%
