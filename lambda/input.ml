(** Abstract syntax of input files. *)

(** Abstract syntax of expressions as given by the user. *)
type expr = expr' * Common.position
and expr' =
  | Var of Common.variable
  | Lambda of Common.variable * expr
  | App of expr * expr

type toplevel = toplevel' * Common.position
and toplevel' =
  | TopDefine of Common.variable * expr
  | TopConstant of Common.variable list
  | Expr of expr
  | Help
  | Quit
  | Context

