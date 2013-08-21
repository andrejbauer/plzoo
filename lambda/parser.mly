%{
  open Input

  (* Build nested lambdas *)
  let make_lambda e xs =
    List.fold_right (fun (x, loc) e -> (Lambda (x, e), loc)) xs e

%}

%token <string> NAME
%token LPAREN RPAREN LAMBDA PERIOD
%token COLONEQ SEMI
%token VAR
%token CONTEXT HELP QUIT
%token EOF

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

%%

(* Toplevel syntax *)

(* If you're going to "optimize" this, please make sure we don't require;; at the
   end of the file. *)
file:
  | lst = file_topdef
    { lst }
  | e = topexpr EOF
     { [e] }
  | e = topexpr SEMI lst = file
     { e :: lst }
  | dir = topdirective EOF
     { [dir] }
  | dir = topdirective SEMI lst = file
     { dir :: lst }

file_topdef:
  | EOF
     { [] }
  | def = topdef SEMI lst = file
     { def :: lst }

commandline:
  | def = topdef SEMI
    { def }
  | e = topexpr SEMI
    { e }
  | dir = topdirective SEMI
    { dir }

topexpr: mark_position(plain_topexpr) { $1 }
plain_topexpr:
  | e = expr
    { Expr e }

(* Things that can be defined on toplevel. *)
topdef: mark_position(plain_topdef) { $1 }
plain_topdef:
  | VAR xs = nonempty_list(NAME)
    { TopParameter xs }
  | x = NAME COLONEQ e = expr
    { TopDefine (x, e) }

(* Toplevel directive. *)
topdirective: mark_position(plain_topdirective) { $1 }
plain_topdirective:
  | CONTEXT
    { Context }
  | HELP
    { Help }
  | QUIT
    { Quit }

(* Main syntax tree *)

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | LAMBDA xs = binders PERIOD e = expr
    { fst (make_lambda e xs) }
  | e = plain_app_expr
    { e }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | x = NAME
    { Var x }
  | LPAREN e = plain_expr RPAREN
    { e }

binders: nonempty_list(name) { $1 }
name: mark_position(plain_name) { $1 }
plain_name:
  | x = NAME
    { x }

mark_position(X):
  x = X
  { x, Common.Position ($startpos, $endpos) }

%%
