%{
  open Input

  (* Build nested lambdas *)
  let make_lambda e xs =
    let rec fold = function
      | [] -> e
      | {Zoo.data=x; loc} :: xs ->
         let e = fold xs in
         Zoo.locate ~loc (Lambda (x, e))
    in
    fold xs
%}

%token <string> NAME
%token LPAREN RPAREN LAMBDA PERIOD
%token COLONEQ SEMI
%token CONTEXT HELP QUIT CONSTANT LAZY EAGER DEEP SHALLOW
%token EOF

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

%%

(* Toplevel syntax *)

(* If you're going to "optimize" this, please make sure we don't require;; at the
   end of the file. *)
file:
  | EOF                          { [] }
  | e=topdef EOF                 { [e] }
  | e=topdef SEMI es=file        { e :: es }
  | e=topexpr EOF                { [e] }
  | e=topexpr SEMI es=file       { e :: es }
  | e=topdirective es=file       { e :: es }
  | e=topdirective SEMI es=file  { e :: es }

commandline:
  | def = topdef EOF
    { def }
  | e = topexpr EOF
    { e }
  | dir = topdirective EOF
    { dir }

topexpr: mark_position(plain_topexpr) { $1 }
plain_topexpr:
  | e = expr
    { Expr e }

(* Things that can be defined on toplevel. *)
topdef: mark_position(plain_topdef) { $1 }
plain_topdef:
  | x = NAME COLONEQ e = expr
    { TopDefine (x, e) }

(* Toplevel directive. *)
topdirective: mark_position(plain_topdirective) { $1 }
plain_topdirective:
  | CONSTANT x=NAME
    { TopConstant x }
  | EAGER
    { Eager true }
  | LAZY
    { Eager false }
  | DEEP
    { Deep true }
  | SHALLOW
    { Deep false }
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
    { (make_lambda e xs).Zoo.data }
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
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%
