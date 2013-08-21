(* The abstract syntax of untyped lambda calculus. *)

(* Expressions *)
type expr =
  | Var of string (* free variable *)
  | Bound of int (* de Bruijn index *)
  | Lam of string * expr (* abstraction, with suggested name of bound variable *)
  | App of expr * expr (* application *)

(* Toplevel entries *)
type toplevel =
  | Definition of name * expr
  | Expr of expr

let next x = x ^ "'"

let rec fresh x bad =
  if not (List.mem x bad) then
    x
  else 
    fresh (next x) bad

let shadow x env = List.filter (fun (y,_) -> x <> y) env

let rec subst s = function
    Id x as e ->
      (try List.assoc x s with Not_found -> e)
  | App (e1, e2) -> App (subst s e1, subst s e2)
  | Pair (e1, e2) -> Pair (subst s e1, subst s e2)
  | Fst e -> Fst (subst s e)
  | Snd e -> Snd (subst s e)
  | Lam (x, e) ->
      let x' = fresh x (List.concat (List.map (fun (_, e) -> fv e) s)) in
	Lam (x', subst ((x, Id x')::s) e)
