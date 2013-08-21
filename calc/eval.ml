(** Evaluation of expressions, given as big step semantics. *)

open Syntax

(** [eval e] evaluates the expression [e] to an integer. It raises an
    expressions if division by zero occurs. *)
let rec eval = function
  | Numeral n -> n
  | Plus (e1, e2) -> eval e1 + eval e2
  | Minus (e1, e2) -> eval e1 - eval e2
  | Times (e1, e2) -> eval e1 * eval e2
  | Divide (e1, e2) ->
      let n2 = eval e2 in
	if n2 <> 0 then eval e1 / n2 else failwith "Division by zero"
  | Negate e -> - (eval e)
