
type name_parts = string list
type t = int * Syntax.operator list
type graph = t list

let string_of_precedence (p, lst) =
  Printf.sprintf "Precedence %d\n  %s" p (
    String.concat "\n  " (List.map Syntax.string_of_op lst)
  )

let string_of_graph (g:graph) =
  String.concat "\n" (List.map string_of_precedence g)

let rec sucs (p:t) = function
  | [] -> []
  | (x, _)::_ as r when x >= (fst p) -> r
  | _::rest -> sucs p rest
;;

let empty_graph = []

let rec update_precedence (operator:Syntax.operator) = function
  | [] -> [(operator.fx, [operator.tokens])]
  | (fx, lst) :: tail when fx = operator.fx -> 
    (fx, operator.tokens :: lst) :: tail
  | head :: tail -> head :: update_precedence operator tail
;;

(* Make sure the graph is always sorted! *)
let rec add_operator (state: graph) prec operator : graph = 
  match state with
  | [] -> [(prec, [operator])]
  | (prec', lst) :: tail -> 
    if prec = prec' then
      (prec', operator::lst) :: tail
    else
      if prec' < prec then
        (prec', lst) :: add_operator tail prec operator
      else
        (prec, [operator]) :: state
