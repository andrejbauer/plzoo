(** Abstract syntax of input files. *)

(** Abstract syntax of expressions as given by the user. *)
type expr = expr' * Zoo.position
and expr' =
  | Var of string
  | Lambda of string * expr
  | App of expr * expr

type toplevel = toplevel' * Zoo.position
and toplevel' =
  | TopDefine of string * expr
  | TopConstant of string list
  | Expr of expr
  | Help
  | Quit
  | Context
  | Eager of bool
  | Deep of bool

