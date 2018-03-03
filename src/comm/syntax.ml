(** Abstract syntax. *)

(** Arithmetical expressions. *)
type expression =
  | Variable of string (* a variable *)
  | Numeral of int (* non-negative integer constant *)
  | Plus of expression * expression  (* Addition [e1 + e2] *)
  | Minus of expression * expression (* Difference [e1 - e2] *)
  | Times of expression * expression (* Product [e1 * e2] *)
  | Divide of expression * expression (* Quotient [e1 / e2] *)
  | Remainder of expression * expression (* Remainder [e1 % e2] *)

(** Boolean expressions. *)
type boolean =
  | True
  | False
  | Equal of expression * expression
  | Less of expression * expression
  | And of boolean * boolean
  | Or of boolean * boolean
  | Not of boolean

(** Commands. *)
type command =
  | Skip
  | New of string * expression * command
  | Print of expression
  | Assign of string * expression
  | Sequence of command * command
  | While of boolean * command
  | Conditional of boolean * command * command
