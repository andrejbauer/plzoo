(** Abstract syntax of input files. *)

(** Abstract syntax of expressions as given by the user. *)
type expr = expr' * Common.position
and expr' =
  | Var of Common.variable
  | Universe of int
  | Pi of abstraction
  | Lambda of abstraction
  | App of expr * expr
 
(** An abstraction [(x,t,e)] indicates that [x] of type [t] is bound in [e]. *)
and abstraction = Common.variable * expr * expr

(** Toplevel directives. *)
type directive = directive' * Common.position
and directive' =
  | Quit
  | Help
  | Context
  | Parameter of Common.variable * expr
  | Definition of Common.variable * expr
  | Check of expr
  | Eval of expr
