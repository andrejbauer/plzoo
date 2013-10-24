(** Abstract syntax. *)

(** Arithmetical expressions. *)
type expression =
  | Variable of string (* a variable *)
  | Numeral of int (* non-negative integer constant *)
  | Plus of expression * expression  (* Addition [e1 + e2] *)
  | Minus of expression * expression (* Difference [e1 - e2] *)
  | Times of expression * expression (* Product [e1 * e2] *)
  | Divide of expression * expression (* Quotient [e1 / e2] *)
  | Negate of expression (* Opposite value [-e] *)

(** Toplevel commands are expressions and definitions. *)
type command =
  | Expression of expression
  | Definition of string * expression

(** Conversion of expresions to strings. *)
let string_of_expression e =
  let rec to_str n e =
    let (m, str) = match e with
      | Variable x      ->    (3, x)
      | Numeral n       ->    (3, string_of_int n)
      | Negate e        ->    (2, "-" ^ (to_str 0 e))
      | Times (e1, e2)  ->    (1, (to_str 1 e1) ^ " * " ^ (to_str 2 e2))
      | Divide (e1, e2) ->    (1, (to_str 1 e1) ^ " / " ^ (to_str 2 e2))
      | Plus (e1, e2)   ->    (0, (to_str 0 e1) ^ " + " ^ (to_str 1 e2))
      | Minus (e1, e2)  ->    (0, (to_str 0 e1) ^ " - " ^ (to_str 1 e2))
    in
      if m < n then "(" ^ str ^ ")" else str
  in
    to_str (-1) e
