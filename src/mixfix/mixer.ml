(* Syntax  *)



let rec expr_d (debug:bool) = function
  | Presyntax.Var varname -> Syntax.Var varname
  | Presyntax.Int k -> Syntax.Int k
  | Presyntax.Bool b -> Syntax.Bool b
  | Presyntax.Times (e1, e2) -> Syntax.Times ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Divide (e1, e2) -> Syntax.Divide ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Mod (e1, e2) -> Syntax.Mod ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Plus (e1, e2) -> Syntax.Plus ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Minus (e1, e2) -> Syntax.Minus ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Equal (e1, e2) -> Syntax.Equal ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Less (e1, e2) -> Syntax.Less ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.If (e1, e2, e3) -> Syntax.If ( expr_d debug e1,  expr_d debug e2,  expr_d debug e3)
  | Presyntax.Fun (x, ht, e) -> Syntax.Fun (x, ht,  expr_d debug e)
  | Presyntax.Seq es -> (if debug then log_seq es else (); seq_to_app debug es)
  | Presyntax.Pair (e1, e2) -> Syntax.Pair ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Fst e -> Syntax.Fst ( expr_d debug e)
  | Presyntax.Snd e -> Syntax.Snd ( expr_d debug e)
  | Presyntax.Rec (f, x, e1) -> Syntax.Rec (f, x,  expr_d debug e1)
  | Presyntax.Nil ht -> Syntax.Nil ht
  | Presyntax.Cons (e1, e2) -> Syntax.Cons ( expr_d debug e1,  expr_d debug e2)
  | Presyntax.Match (e, ht, e1, x, y, e2) -> Syntax.Match ( expr_d debug e, ht,  expr_d debug e1, x, y,  expr_d debug e2)
      (** list decomposition [match e with [t] -> e1 | x::y -> e2] *)

and seq_to_app (debug:bool) (es: Presyntax.expr list): Syntax.expr =
  (* Fold left wants initial value, which we dont really have? *)
  match es with
    | [] -> failwith "seq should be non-empty" 
    | a :: [] -> expr_d debug a
    | head :: tail -> Syntax.Apply (expr_d debug head, seq_to_app debug tail)
and
  log_seq (seq:Presyntax.expr list) =
  let rec log_seq' (seq:Presyntax.expr list) =
    match seq with
    | [] -> ()
    | head :: tail -> print_string ((Syntax.string_of_expr (expr_d false head)) ^ ";"); log_seq' tail
  in
  print_string "seq [";
  log_seq' seq;
  print_string "]\n"
;;

let expr = expr_d true
;;

let toplevel_cmd (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
  match cmd with
  | Presyntax.Expr e -> Syntax.Expr (expr e)
  | Presyntax.Def (name,  e) -> Syntax.Def (name, expr e)
  | Presyntax.MixDef (mixassoc,mixname, prec, e) -> Syntax.MixDef (mixassoc, mixname, prec, (expr e) )
  | Presyntax.Quit -> Syntax.Quit
;;

let file = List.map toplevel_cmd
;;