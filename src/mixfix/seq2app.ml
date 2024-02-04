

type state = Syntax.operator list

let add_operator state operator = operator :: state
;;

let parse_presyntax (s:state) (presyn_expr) = 
  let rec expr = function
  | Presyntax.Var varname -> Syntax.Var varname
  | Presyntax.Int k -> Syntax.Int k
  | Presyntax.Bool b -> Syntax.Bool b
  | Presyntax.Times (e1, e2) -> Syntax.Times ( expr e1,  expr e2)
  | Presyntax.Divide (e1, e2) -> Syntax.Divide ( expr e1,  expr e2)
  | Presyntax.Mod (e1, e2) -> Syntax.Mod ( expr e1,  expr e2)
  | Presyntax.Plus (e1, e2) -> Syntax.Plus ( expr e1,  expr e2)
  | Presyntax.Minus (e1, e2) -> Syntax.Minus ( expr e1,  expr e2)
  | Presyntax.Equal (e1, e2) -> Syntax.Equal ( expr e1,  expr e2)
  | Presyntax.Less (e1, e2) -> Syntax.Less ( expr e1,  expr e2)
  | Presyntax.If (e1, e2, e3) -> Syntax.If ( expr e1,  expr e2,  expr e3)
  | Presyntax.Fun (x, ht, e) -> Syntax.Fun (x, ht,  expr e)
  | Presyntax.Seq es -> (seq_to_app es)
  | Presyntax.Pair (e1, e2) -> Syntax.Pair ( expr e1,  expr e2)
  | Presyntax.Fst e -> Syntax.Fst ( expr e)
  | Presyntax.Snd e -> Syntax.Snd ( expr e)
  | Presyntax.Rec (f, x, e1) -> Syntax.Rec (f, x,  expr e1)
  | Presyntax.Nil ht -> Syntax.Nil ht
  | Presyntax.Cons (e1, e2) -> Syntax.Cons ( expr e1,  expr e2)
  | Presyntax.Match (e, ht, e1, x, y, e2) -> Syntax.Match ( expr e, ht,  expr e1, x, y,  expr e2)
and seq_to_app (es: Presyntax.expr list): Syntax.expr =
  (* Fold left wants initial value, which we dont really have? *)
  match es with
    | [] -> failwith "seq should be non-empty" 
    | a :: [] -> expr a
    | head :: tail -> Syntax.Apply (expr head, seq_to_app tail)
in 
  expr presyn_expr
;;