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
  | Do of name * expr * expr  	  (** sequencing [do x <- e1 in e2] *)
  | Let of name * value * expr   (** let-binding [let x = v in e] *)
  | If of value * expr * expr  	  (** conditional [if v then e1 else e2] *)
  | Fun of name * ltype * expr 	  (** function [fun x:s -> e] *)
  | Apply of expr * value      	  (** application [e v] *)
  | Rec of name * ltype * expr 	  (** recursion [rec x : t is e] *)

(** Toplevel commands *)
type toplevel =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
