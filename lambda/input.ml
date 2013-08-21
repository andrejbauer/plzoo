(* The abstract syntax as parsed by the parser *)

type expr =
  | Var of string
  | App of expr * expr
  | Lambda of string * expr

