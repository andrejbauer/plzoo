(** Abstract syntax *)

type associativity = LeftAssoc | RightAssoc | NonAssoc

type fixity = Prefix | Postfix | Infix of associativity | Closed

type operator = {
  tokens: string list;
  fx : fixity;
  prec: int;
}


(** The type of variable names. *)
type name = string

(** Mixfix expressions *)
type expr =
  | Var of name          (** variable *)
  | Int of int           (** integer constant *)
  | Bool of bool         (** boolean constant *)
  | Times of expr * expr (** product [e1 * e2] *)
  | Divide of expr * expr(** quotient [e1 / e2] *)
  | Mod of expr * expr   (** remainder [e1 % e2] *)
  | Plus of expr * expr  (** sum [e1 + e2] *)
  | Minus of expr * expr (** difference [e1 - e2] *)
  | Equal of expr * expr (** integer equality [e1 = e2] *)
  | Less of expr * expr  (** integer comparison [e1 < e2] *)
  | If of expr * expr * expr (** conditional [if e1 then e2 else e3] *)
  | Fun of name * Presyntax.htype * expr (** function [fun x:t -> e] *)
  | Apply of expr * expr (** application [e1 e2] *)
  | Pair of expr * expr  (** pair [(e1, e2)] *)
  | Fst of expr          (** first projection [fst e] *)
  | Snd of expr          (** second projection [snd e] *)
  | Rec of name * Presyntax.htype * expr (** recursion [rec x:t is e] *)
  | Nil of Presyntax.htype         (** empty list *)
  | Cons of expr * expr  (** cons list [e1 :: e2] *)
  | Match of expr * Presyntax.htype * expr * name * name * expr
      (** list decomposition [match e with [t] -> e1 | x::y -> e2] *)

(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
  | Mixfix of operator
  | Quit

let predef_cascade rec_f = function
  | Presyntax.Int k -> Int k
  | Presyntax.Bool b -> Bool b
  | Presyntax.Nil ht -> Nil ht
  | Presyntax.Times (e1, e2) -> Times ( rec_f e1,  rec_f e2)
  | Presyntax.Divide (e1, e2) -> Divide ( rec_f e1,  rec_f e2)
  | Presyntax.Mod (e1, e2) -> Mod ( rec_f e1,  rec_f e2)
  | Presyntax.Plus (e1, e2) -> Plus ( rec_f e1,  rec_f e2)
  | Presyntax.Minus (e1, e2) -> Minus ( rec_f e1,  rec_f e2)
  | Presyntax.Equal (e1, e2) -> Equal ( rec_f e1,  rec_f e2)
  | Presyntax.Less (e1, e2) -> Less ( rec_f e1,  rec_f e2)
  | Presyntax.If (e1, e2, e3) -> If ( rec_f e1,  rec_f e2,  rec_f e3)
  | Presyntax.Fun (x, ht, e) -> Fun (x, ht,  rec_f e)
  | Presyntax.Pair (e1, e2) -> Pair ( rec_f e1,  rec_f e2)
  | Presyntax.Fst e -> Fst ( rec_f e)
  | Presyntax.Snd e -> Snd ( rec_f e)
  | Presyntax.Rec (f, x, e1) -> Rec (f, x,  rec_f e1)
  | Presyntax.Cons (e1, e2) -> Cons ( rec_f e1,  rec_f e2)
  | Presyntax.Match (e, ht, e1, x, y, e2) -> Match ( rec_f e, ht,  rec_f e1, x, y,  rec_f e2)
  

(** Conversion from an expression to a string *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
	  Int n ->          (10, string_of_int n)
	| Bool b ->         (10, string_of_bool b)
	| Var x ->          (10, x)
	| Pair (e1, e2) ->  (10, "(" ^ (to_str 0 e1) ^ ", " ^ (to_str 0 e2) ^ ")")
	| Nil ty ->         (10, "[" ^ (Presyntax.string_of_type ty) ^ "]")
	| Fst e ->           (9, "fst " ^ (to_str 9 e))
	| Snd e ->           (9, "snd " ^ (to_str 9 e))
	| Apply (e1, e2) ->   
    (* (9, "<app>") *)
	    (9, (to_str 8 e1) ^ " " ^ (to_str 9 e2))
	| Times (e1, e2) ->  (9, (to_str 8 e1) ^ " * " ^ (to_str 8 e2))
	| Divide (e1, e2) -> (9, (to_str 8 e1) ^ " / " ^ (to_str 8 e2))
	| Mod (e1, e2) ->    (9, (to_str 8 e1) ^ " % " ^ (to_str 8 e2))
	| Plus (e1, e2) ->   (9, (to_str 8 e1) ^ " + " ^ (to_str 8 e2))
	| Minus (e1, e2) ->  (9, (to_str 8 e1) ^ " - " ^ (to_str 8 e2))
	| Cons (e1, e2) ->   (9, (to_str 8 e1) ^ " :: " ^(to_str 8 e2))
	| Equal (e1, e2) ->  (9, (to_str 8 e1) ^ " = " ^ (to_str 8 e2))
	| Less (e1, e2) ->   (9, (to_str 8 e1) ^ " < " ^ (to_str 8 e2))
	| If (e1, e2, e3) -> (9, "if " ^ (to_str 8 e1) ^ " then " ^
				(to_str 8 e2) ^ " else " ^ (to_str 8 e3))
	| Match (e1, ty, e2, x, y, e3) ->
	    (3, "match " ^ (to_str 3 e1) ^ " with " ^
	       "[" ^ (Presyntax.string_of_type ty) ^ "] -> " ^ (to_str 3 e2) ^ " | " ^
	       x ^ "::" ^ y ^ " -> " ^ (to_str 3 e3))
	| Fun (x, ty, e) -> 
    (* (10, "<fun>") *)
	    (2, "fun " ^ x ^ " : " ^ (Presyntax.string_of_type ty) ^ " -> " ^ (to_str 0 e))
	| Rec (x, ty, e) -> 
    (* (10, "<rec>") *)
	    (1, "rec " ^ x ^ " : " ^ (Presyntax.string_of_type ty) ^ " is " ^ (to_str 0 e))

    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e

(** [subst [(x1,e1);...;(xn;en)] e] replaces in [e] free occurrences
    of variables [x1], ..., [xn] with expressions [e1], ..., [en]. *)
let rec subst s = function
  |  (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _ | Nil _) as e -> e
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Divide (e1, e2) -> Divide (subst s e1, subst s e2)
  | Mod (e1, e2) -> Mod (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Cons (e1, e2) -> Cons  (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (x, ty, e) -> let s' = List.remove_assoc x s in Fun (x, ty, subst s' e)
  | Rec (x, ty, e) -> let s' = List.remove_assoc x s in Rec (x, ty, subst s' e)
  | Match (e1, ty, e2, x, y, e3) ->
      let s' = List.remove_assoc y (List.remove_assoc x s) in
	Match (subst s e1, ty, subst s e2, x, y, subst s' e3)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Pair (e1, e2) -> Pair (subst s e1, subst s e2)
  | Fst e -> Fst (subst s e)
  | Snd e -> Snd (subst s e)
