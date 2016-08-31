(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(** Levy types are separated into value types and computation types, but
    it is convenient to have a single datatype for all of them. *)
type ltype = ltype' Zoo.located
and ltype' =
  | VInt                     (** integer [int] *)
  | VBool                    (** booleans [bool] *)
  | VForget of ctype         (** thunked type [U t] *)
  | CFree of vtype           (** free type [F s] *)
  | CArrow of vtype * ctype  (** Function type [s -> t] *)

and vtype = ltype

and ctype = ltype

(** Levy expressions. We actually use the same type for values
    and computations because it makes the code shorter and produces
    more reasonable error messages during type checking. *)

type value = expr

and expr = expr' Zoo.located
and expr' =
  | Var of name            	  (** variable *)
  | Int of int             	  (** integer constant *)
  | Bool of bool           	  (** boolean constant *)
  | Times of value * value 	  (** product [v1 * v2] *)
  | Plus of value * value  	  (** sum [v1 + v2] *)
  | Minus of value * value 	  (** difference [v1 - v2] *)
  | Equal of value * value 	  (** integer equality [v1 = v2] *)
  | Less of value * value  	  (** integer comparison [v1 < v2] *)
  | Thunk of expr          	  (** thunk [thunk e] *)
  | Force of value             	  (** [force v] *)
  | Return of value            	  (** [return v] *)
  | To of expr * name * expr  	  (** sequencing [e1 to x . e2] *)
  | Let of name * value * expr   (** let-binding [let x = v in e] *)
  | If of value * expr * expr  	  (** conditional [if v then e1 else e2] *)
  | Fun of name * ltype * expr 	  (** function [fun x:s -> e] *)
  | Apply of expr * value      	  (** application [e v] *)
  | Rec of name * ltype * expr 	  (** recursion [rec x : t is e] *)

(** Toplevel commands *)
type toplevel =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)

(** Conversion from a type to a string *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty.Zoo.data with
	| VInt -> (3, "int")
	| VBool -> (3, "bool")
	| VForget ty -> (2, "U " ^ to_str 1 ty)
	| CFree ty -> (2, "F " ^ to_str 1 ty)
	| CArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(** Conversion from an expression to a string *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e.Zoo.data with
    | Int n ->           (10, string_of_int n)
    | Bool b ->          (10, string_of_bool b)
    | Var x ->           (10, x)
    | Return e ->        ( 9, "return " ^ (to_str 9 e))
    | Force e ->         ( 9, "force " ^ (to_str 9 e))
    | Thunk e ->         ( 9, "thunk " ^ (to_str 9 e))
    | Apply (e1, e2) ->  ( 9, (to_str 8 e1) ^ " " ^ (to_str 9 e2))
    | Times (e1, e2) ->  ( 8, (to_str 7 e1) ^ " * " ^ (to_str 8 e2))
    | Plus (e1, e2) ->   ( 7, (to_str 6 e1) ^ " + " ^ (to_str 7 e2))
    | Minus (e1, e2) ->  ( 7, (to_str 6 e1) ^ " - " ^ (to_str 7 e2))
    | Equal (e1, e2) ->  ( 5, (to_str 5 e1) ^ " = " ^ (to_str 5 e2))
    | Less (e1, e2) ->   ( 5, (to_str 5 e1) ^ " < " ^ (to_str 5 e2))
    | If (e1, e2, e3) -> ( 4, "if " ^ (to_str 4 e1) ^ " then " ^ (to_str 4 e2) ^ " else " ^ (to_str 4 e3))
    | Fun (x, ty, e) ->  ( 2, "fun " ^ x ^ " : " ^ (string_of_type ty) ^ " -> " ^ (to_str 0 e))
    | Rec (x, ty, e) ->  ( 2, "rec " ^ x ^ " : " ^ (string_of_type ty) ^ " is " ^ (to_str 0 e))
    | Let (x, e1, e2) ->( 1, "let " ^ x ^ " = " ^ to_str 1 e1 ^ " in " ^ to_str 0 e2)
    | To (e1, x, e2) ->  ( 1, to_str 1 e1 ^ " to " ^ x ^ " . " ^ to_str 0 e2)
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e
