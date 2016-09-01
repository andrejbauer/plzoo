(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(** Types in Poly. *)
type htype =
  | TInt                     (** integers [int] *)
  | TBool                    (** booleans [bool] *)
  | TParam of int            (** parameter *)
  | TTimes of htype * htype  (** Product [s * t] *)
  | TArrow of htype * htype  (** Function type [s -> t] *)
  | TList of htype           (** Lists *)

(** Poly expressions. *)
type expr =
  | Var of name          (* variable *)
  | Int of int           (* integer constant *)
  | Bool of bool         (* boolean value *)
  | Times of expr * expr (* product [e1 * e2] *)
  | Divide of expr * expr(* quotient [e1 / e2] *)
  | Mod of expr * expr   (* remainder [e1 % e2] *)
  | Plus of expr * expr  (* sum [e1 + e2] *)
  | Minus of expr * expr (* difference [e1 - e2] *)
  | Equal of expr * expr (* integer equality [e1 = e2] *)
  | Less of expr * expr  (* integer comparison [e1 < e2] *)
  | If of expr * expr * expr (* conditional [if e1 then e2 else e3] *)
  | Fun of name * expr   (* function [fun x -> e] *)
  | Apply of expr * expr (* application [e1 e2] *)
  | Pair of expr * expr  (* ordered pair [(e1, e2)] *)
  | Fst of expr          (* first projection [fst e] *)
  | Snd of expr          (* second projection [snd e] *)
  | Rec of name * expr   (* recursion [rec x is e] *)
  | Nil                  (* empty list *)
  | Cons of expr * expr  (* cons list *)
  | Match of expr * expr * name * name * expr
      (* list decomposition [match e with [] -> e1 | x::y -> e2] *)

(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (* an expression to be evaluated *)
  | Def of name * expr (* toplevel definition [let x = e] *)

(** [rename t] renames parameters in type [t] so that they count from
    [0] up. This is useful for pretty printing. *)
let rename ty =
  let rec ren ((j,s) as c) = function
    | TInt -> TInt, c
    | TBool -> TBool, c
    | TParam k ->
	(try
	   TParam (List.assoc k s), c
	 with 
	     Not_found -> TParam j, (j+1, (k, j)::s))
    | TArrow (t1, t2) ->
	let u1, c'  = ren c t1 in
	let u2, c'' = ren c' t2 in
	  TArrow (u1,u2), c''
    | TTimes (t1, t2) ->
	let u1, c'  = ren c t1 in
	let u2, c'' = ren c' t2 in
	  TTimes (u1,u2), c''
    | TList t ->
	let u, c' = ren c t in TList u, c'
  in
    fst (ren (0,[]) ty)

(** [rename t1 t2] simultaneously renames types [t1] and [t2] so that
    parameters appearing in them are numbered from [0] on. *)
let rename2 t1 t2 =
  match rename (TTimes (t1,t2)) with
      TTimes (u1, u2) -> u1, u2
    | _ -> assert false

(** [string_of_type] converts a Poly type to string. *)
let string_of_type ty =
  let a = [|"a";"b";"c";"d";"e";"f";"g";"h";"i";
	    "j";"k";"l";"m";"n";"o";"p";"q";"r";
	    "s";"t";"u";"v";"w";"x";"y";"z"|]
  in
  let rec to_str n ty =
    let (m, str) =
      match ty with
	| TInt -> (4, "int")
	| TBool -> (4, "bool")
	| TParam k -> (4, (if k < Array.length a then "'" ^ a.(k) else "'ty" ^ string_of_int k))
	| TList ty -> (3, to_str 3 ty ^ " list")
	| TTimes (ty1, ty2) -> (2, (to_str 2 ty1) ^ " * " ^ (to_str 2 ty2))
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(** [string_of_expr e] converts expression [e] to string. *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
	  Int n ->          (10, string_of_int n)
	| Bool b ->         (10, string_of_bool b)
	| Var x ->          (10, x)
	| Pair (e1, e2) ->  (10, "(" ^ (to_str 0 e1) ^ ", " ^ (to_str 0 e2) ^ ")")
	| Nil ->            (10, "[]")
	| Fst e ->           (9, "fst " ^ (to_str 9 e))
	| Snd e ->           (9, "snd " ^ (to_str 9 e))
	| Apply (e1, e2) ->  (10, "<app>")
	    (* (9, (to_str 8 e1) ^ " " ^ (to_str 9 e2)) *)
	| Times (e1, e2) ->  (8, (to_str 7 e1) ^ " * " ^ (to_str 8 e2))
	| Divide (e1, e2) -> (8, (to_str 7 e1) ^ " / " ^ (to_str 8 e2))
	| Mod (e1, e2) ->    (8, (to_str 7 e1) ^ " % " ^ (to_str 8 e2))
	| Plus (e1, e2) ->   (7, (to_str 6 e1) ^ " + " ^ (to_str 7 e2))
	| Minus (e1, e2) ->  (7, (to_str 6 e1) ^ " - " ^ (to_str 7 e2))
	| Cons (e1, e2) ->   (6, (to_str 6 e1) ^ " :: " ^ (to_str 5 e2))
	| Equal (e1, e2) ->  (5, (to_str 5 e1) ^ " = " ^ (to_str 5 e2))
	| Less (e1, e2) ->   (5, (to_str 5 e1) ^ " < " ^ (to_str 5 e2))
	| If (e1, e2, e3) -> (4, "if " ^ (to_str 4 e1) ^ " then " ^
				(to_str 4 e2) ^ " else " ^ (to_str 4 e3))
	| Match (e1, e2, x, y, e3) ->
	    (3, "match " ^ (to_str 3 e1) ^ " with " ^
	       "[] -> " ^ (to_str 3 e2) ^ " | " ^
	       x ^ "::" ^ y ^ " -> " ^ (to_str 3 e3))
	| Fun (x, e) -> (10, "<fun>")
	    (* (2, "fun " ^ x ^  " -> " ^ (to_str 0 e)) *)
	| Rec (x, e) -> (10, "<rec>")
	    (* (1, "rec " ^ x ^ " is " ^ (to_str 0 e)) *)
	       
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e

(** [subst [(x1,e1);...;(xn;en)] e] replaces in [e] free occurrences
    of variables [x1], ..., [xn] with expressions [e1], ..., [en]. *)
let rec subst s = function
  | (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _ | Nil) as e -> e
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Divide (e1, e2) -> Divide (subst s e1, subst s e2)
  | Mod (e1, e2) -> Mod (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Cons (e1, e2) -> Cons  (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (x, e) -> let s' = List.remove_assoc x s in Fun (x, subst s' e)
  | Rec (x, e) -> let s' = List.remove_assoc x s in Rec (x, subst s' e)
  | Match (e1, e2, x, y, e3) ->
      let s' = List.remove_assoc y (List.remove_assoc x s) in
	Match (subst s e1, subst s e2, x, y, subst s' e3)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Pair (e1, e2) -> Pair (subst s e1, subst s e2)
  | Fst e -> Fst (subst s e)
  | Snd e -> Snd (subst s e)

(** [tsubst [(k1,t1); ...; (kn,tn)] t] replaces in type [t] parameters
    [TParam ki] with types [ti]. *)
let rec tsubst s = function
  | (TInt | TBool) as t -> t
  | TParam k -> (try List.assoc k s with Not_found -> TParam k)
  | TTimes (t1, t2) -> TTimes (tsubst s t1, tsubst s t2)
  | TArrow (t1, t2) -> TArrow (tsubst s t1, tsubst s t2)
  | TList t -> TList (tsubst s t)
