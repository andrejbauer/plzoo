(** Abstract syntax of input files. *)

(** Abstract syntax of expressions as given by the user. *)
type expr = expr' * Common.position
and expr' =
  | Var of Common.variable
  | Type
  | Id of expr * expr * sort
  | Refl of expr
  | Transport of expr * expr * expr
  | Nat
  | Numeral of int
  | Succ of expr
  | NatRec of expr * expr * expr * expr
  | Pi of Common.variable * sort * expr
  | Lambda of Common.variable * sort option * expr
  | App of expr * expr
  | Ascribe of expr * sort
  | TyJdg of expr * sort
  | EqJdg of expr * expr * sort

and sort = expr

type operation = operation' * Common.position
and operation' =
  | Inhabit of sort
  | Infer of expr
  | HasType of expr * sort
  | Equal of expr * expr * sort

type computation = computation' * Common.position
and computation' = 
  | Return of expr
  | Abstraction of Common.variable * sort * computation
  | Operation of operation
  | Handle of computation * handler
  | Let of Common.variable * computation * computation

and handler =
    (expr * expr * sort * computation) list

(** Toplevel directives. *)
type directive = directive' * Common.position
and directive' =
  | Quit
  | Help
  | Context
  | Eval of expr

type toplevel = toplevel' * Common.position
and toplevel' =
  | Computation of computation
  | TopDefine of Common.variable * expr
  | TopLet of Common.variable * computation
  | TopParam of Common.variable list * sort
  | Context
  | Eval of expr
  | Help
  | Quit

