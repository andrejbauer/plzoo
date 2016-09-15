%{
  open Input

  (* Build nested lambdas *)
  let rec make_lambda e = function
    | [] -> e
    | ((xs, t), loc) :: lst ->
      let e = make_lambda e lst in
        List.fold_right (fun x e -> (Lambda (x, t, e), loc)) xs e

  (* Build nested pies *)
  let rec make_pi e = function
    | [] -> e
    | ((xs, t), loc) :: lst ->
      let e = make_pi e lst in
        List.fold_right (fun x e -> (Pi (x, t, e), loc)) xs e

%}

%token FORALL FUN TYPE
%token <int> NUMERAL
%token <string> NAME
%token LPAREN RPAREN
%token COLON COMMA PERIOD COLONEQUAL
%token ARROW DARROW
%token QUIT HELP PARAMETER CHECK EVAL CONTEXT DEFINITION
%token EOF

%start <Input.directive list> directives

%%

(* Toplevel syntax *)

directives:
  | dir = directive PERIOD EOF
     { [dir] }
  | dir = directive PERIOD lst = directives
     { dir :: lst }

directive: mark_position(plain_directive) { $1 }
plain_directive:
  | QUIT
    { Quit }
  | HELP
    { Help }
  | PARAMETER x = NAME COLON e = expr
    { Parameter (x, e) }
  | CHECK e = expr
    { Check e }
  | EVAL e = expr
    { Eval e}
  | DEFINITION x = NAME COLONEQUAL e = expr
    { Definition (x, e) }
  | CONTEXT
    { Context }

(* Main syntax tree *)
expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | FORALL lst = abstraction COMMA e = expr
    { fst (make_pi e lst) }
  | FUN lst = abstraction DARROW e = expr
    { fst (make_lambda e lst) }
  | t1 = app_expr ARROW t2 = expr
    { Pi ("_", t1, t2) }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | TYPE k = NUMERAL
    { Universe k }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | n = NAME
    { Var n }
  | LPAREN e = plain_expr RPAREN
    { e }

abstraction:
  | b = bind1
    { [b] }
  | bs = binds
    { bs }

bind1: mark_position(plain_bind1) { $1 }
plain_bind1:
  | xs = nonempty_list(NAME) COLON t = expr
    { (xs, t) }

binds:
  | LPAREN b = bind1 RPAREN
    { [b] }
  | LPAREN b = bind1 RPAREN lst = binds
    { b :: lst }

mark_position(X):
  x = X
  { x, Common.Position ($startpos, $endpos) }

%%
