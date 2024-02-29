(** Abstract syntax *)

type associativity = LeftAssoc | RightAssoc | NonAssoc

let string_of_assoc = function
  | LeftAssoc -> "left"
  | RightAssoc -> "right"
  | NonAssoc -> "non"

(** The type of fixities. *)

type fixity = Prefix | Postfix | Infix of associativity | Closed

let string_of_fixity = function
  | Prefix -> "prefix"
  | Postfix -> "postfix"
  | Infix LeftAssoc -> "infixl"
  | Infix RightAssoc -> "infixr"
  | Infix NonAssoc -> "infix"
  | Closed -> "closed"

(** The type of operators. *)

type operator = {
  tokens: string list;
  fx : fixity;
}

let op_name {fx;tokens} =
  let tokens = String.concat "_" tokens in
  match fx with
  | Prefix -> tokens ^ "_"
  | Postfix -> "_" ^ tokens
  | Infix assoc -> "_" ^ tokens ^ "_"
  | Closed -> tokens

let string_of_op ({fx;tokens} as op) = 
  (op_name op) ^ " ("^(string_of_fixity fx) ^ " fixity)"

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
  | Mixfix of int * operator
  | Quit

let rec make_app head = function
  | [] -> head
  | arg::args -> make_app (Apply (head, arg)) args 

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