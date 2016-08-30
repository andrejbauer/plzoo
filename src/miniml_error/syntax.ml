(* Abstract syntax. *)

(* Variable names *)
type name = string

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TArrow of ty * ty (* Functions *)

(* Expressions *)
type expr = expr' Zoo.located
and expr' =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Division of expr * expr             (* Division [e1/e2], may result in [Abort] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * ty * expr (* Function [fun f(x:s):t is e] *)
  | Apply of expr * expr 		(* Application [e1 e2] *)
  | Abort                               (* Special value indicating an error *)

(* Toplevel commands *)
type command =
  | Expr of expr       (* Expression *)
  | Def of name * expr (* Value definition [let x = e] *)
