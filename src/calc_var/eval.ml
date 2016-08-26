(** Evaluation of expressions, given as big step semantics. *)

open Syntax

(** [eval env e] evaluates the expression [e] to an integer,
    where [env] is an association list mapping variables to their values.
    It raises an expressions if division by zero occurs. *)
let eval env =
  let rec eval = function
    | Variable x ->
      (try
         List.assoc x env
       with
         | Not_found -> Zoo.error "unknown variable %s" x)
    | Numeral n -> n
    | Plus (e1, e2) -> eval e1 + eval e2
    | Minus (e1, e2) -> eval e1 - eval e2
    | Times (e1, e2) -> eval e1 * eval e2
    | Divide (e1, e2) ->
      let n2 = eval e2 in
        if n2 <> 0 then eval e1 / n2 else Zoo.error "division by zero"
    | Negate e -> - (eval e)
  in
    eval
