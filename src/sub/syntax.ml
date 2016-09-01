(** Abstract syntax *)

(** Type type of variable names *)
type name = string

(** The type of field names *)
type label = string

(** Types *)
type ty =
  | TInt (** integers [int] *)
  | TBool (** boolean values [bool] *)
  | TArrow of ty * ty (** function types [ty1 -> ty2] *)
  | TRecord of (label * ty) list (** records [{l1:ty1, ..., lN:tyN}] *)

(** Expressions *)
type expr =
  | Var of name (** variable *)
  | Int of int (** integer constant *)
  | Plus of expr * expr (** sum [e1 + e2] *)
  | Minus of expr * expr (** difference [e1 - e2] *)
  | Times of expr * expr (** products [e1 * e2] *)
  | Divide of expr * expr (** quotient [e1 / e2] *)
  | Bool of bool (** boolean constant [true] or [false] *)
  | Equal of expr * expr (** integer equality [e1 = e2] *)
  | Less of expr * expr (** integer comparison [e1 < e2] *)
  | And of expr * expr (** conjunction [e1 and e2] *)
  | Or of expr * expr (** disjunction [e1 or e2] *)
  | Not of expr (** negation [not e] *)
  | If of expr * expr * expr (** conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * ty * expr (** recursive function [fun f(x : ty1):ty2 is e] *)
  | Closure of environment * name * expr (** closure (internal value) *)
  | Let of name * expr * expr (** local definition [let x = e1 in e2] *)
  | App of expr * expr (** application [e1 e2] *)
  | Record of (label * expr) list (** record [{l1=e1, ..., lN=eN}] *)
  | Project of expr * label (** field projection [e.l] *)

(** An environment is an associative list [(x1,v1);...;(xN,vN)]. *)
and environment = (name * expr) list

(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr (** an expression to be evaluated *)
  | Def of name * expr (** Global definition [let x = e] *)

(** [string_of_type ty] converts type [ty] to a string. *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty with
	| TInt -> (4, "int")
	| TBool -> (4, "bool")
	| TRecord ts ->
	    (4, "{" ^
	       String.concat ", "
	       (List.map (fun (l,t) -> l ^ " : " ^ (to_str (-1) t)) ts) ^
	       "}")
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(** [string_of_value v] converts a value [v] to a string. *)
let rec string_of_value = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Record rs ->
      "{" ^ String.concat ", "
	(List.map (fun (l,e) -> l ^ " = " ^ (string_of_value e)) rs) ^
	"}"
  | Closure _ -> "<fun>"
  | _ -> assert false
