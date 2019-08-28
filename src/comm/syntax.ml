(** Abstract syntax. *)

(** Arithmetical expressions. *)
type expression =
  | Variable of string (* a variable *)
  | Numeral of int (* integer constant *)
  | Plus of expression * expression  (* addition [e1 + e2] *)
  | Minus of expression * expression (* difference [e1 - e2] *)
  | Times of expression * expression (* product [e1 * e2] *)
  | Divide of expression * expression (* quotient [e1 / e2] *)
  | Remainder of expression * expression (* remainder [e1 % e2] *)

(** Boolean expressions. *)
type boolean =
  | True (* constant [true] *)
  | False (* constant [false] *)
  | Equal of expression * expression (* equal [e1 = e2] *)
  | Less of expression * expression (* less than [e1 < e2] *)
  | And of boolean * boolean (* conjunction [b1 and b2] *)
  | Or of boolean * boolean (* disjunction [b1 or b2] *)
  | Not of boolean (* negation [not b] *)

(** Commands. *)
type command =
  | Skip (* no operation [skip] *)
  | New of string * expression * command (* variable declaration [new x := e in c] *)
  | Print of expression (* print expression [print e] *)
  | Assign of string * expression (* assign a variable [x := e] *)
  | Sequence of command * command (* sequence commands [c1 ; c2] *)
  | While of boolean * command (* loop [while b do c done] *)
  | Conditional of boolean * command * command (* conditional [if b then c1 else c2 end] *)
