(* Abstract syntax. *)

(* Variable names *)
type name = string

(* Types of "wires" *)
type ty =
  | TyInt
  | TyBool
  | TyString

(* Expressions *)
type expr =
  | Var of name          		(* Variable: x, y, ... *)
  | Int of int           		(* Integer constant: ..., -2, -1, 0, 1, 2, ... *)
  | Bool of bool                        (* Boolean constant: false, true *)
  | String of string                    (* String: "abcdef..." *)
  | Cond of expr * expr * expr          (* Conditional statement: if e1 then e2 else e3 *)
  | Apply of name * expr list 		(* Application: f (e1, ..., en) *)
  | Let of name * expr * expr           (* Local definition "let x = e1 in e2"  *)
  | Copy of expr * name * name * expr   (* Copy an expression "copy e1 as x, y in e2" *)

(* Toplevel definition *)
type def = name * (ty * name) list * expr      (* box f (t1 x1, ..., tn xn) = e *)

type toplevel_cmd =
  | Expr of expr
  | Def of def

(* Convert a type to string *)
let rec string_of_ty = function
  | TyBool -> "bool"
  | TyInt -> "int"
  | TyString -> "string"

let rec string_of_func_ty ts t =
  String.concat " * " (List.map string_of_ty ts) ^ " -> " ^ string_of_ty t

(* Convert an expression to string *)
let string_of_expr e =
	let rec to_str n e =
		let (m, str) =
			match e with
			| Var x -> (7, x)
			| Int n -> (7, string_of_int n)
			| Bool b -> (7, string_of_bool b)
			| String s -> (7, "\""^s^"\"")
			| Apply (f, lst) -> (7, f ^ "(" ^ String.concat "," (List.map (to_str 2) lst) ^ ")")
			| Cond (e1, e2, e3) -> (2, "if " ^ to_str 3 e1 ^ " then " ^ to_str 3 e2 ^ " else " ^ to_str 3 e3)
			| Let (x, e1, e2) -> (2, "let " ^ x ^ " = " ^ to_str 3 e1 ^ " in " ^ to_str 2 e2)
			| Copy (e1, x, y, e2) -> (2, "copy " ^ to_str 3 e1 ^ " as " ^ x ^ "," ^ y ^ " in " ^ to_str 2 e2)
		in
		if m > n then str else "(" ^ str ^ ")"
	in
	to_str (-1) e

(* Convert a definition to a string *)
let string_of_def (f, xs, e) =
  "box " ^ f ^ "(" ^ String.concat "," xs ^ ") = " ^ string_of_expr e

(* [subst [(x1,e1);...;(xn;en)] e] replaces in expression [e] all
    free occurrences of variables [x1], ..., [xn] with expressions
    [e1], ..., [en], respectively. *)
let rec subst s = function
  | (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _ | String _) as e -> e
  | Cond (e1, e2, e3) -> Cond (subst s e1, subst s e2, subst s e3)
  | Apply (f, lst) -> Apply (f, List.map (subst s) lst)
  | Let (x, e1, e2) -> Let (x, subst s e1, subst ((x, Var x) :: s) e2)
  | Copy (e1, x, y, e2) -> Copy (subst s e1, x, y, subst ((x,Var x) :: (y, Var y) :: s) e2)

(* [count x e] counts the number of free occurrences of variable [x] in
   expression [e]. *)
let rec count x = function
  | Var y -> if x = y then 1 else 0
  | (Int _ | Bool _ | String _) -> 0
  | Apply (_, lst) -> List.fold_left (+) 0 (List.map (count x) lst)
  | Cond (e1, e2, e3) -> count x e1 + count x e2 + count x e3
  | Let (y, e1, e2) -> count x e1 + (if x = y then 0 else count x e2)
  | Copy (e1, y, z, e2) -> count x e1 + (if x = y || x = z then 0 else count x e2)
