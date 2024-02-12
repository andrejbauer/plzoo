
type tokens = string list

type precedence = int *  (Syntax.fixity * (tokens list)) list
type t = precedence (* so it is idiomatic :) *)
type graph = precedence list

let new_graph () = []

let rec update_precedence (operator:Syntax.operator) = function
  | [] -> [(operator.fx, [operator.tokens])]
  | (fx, lst) :: tail when fx = operator.fx -> 
    (fx, operator.tokens :: lst) :: tail
  | head :: tail -> head :: update_precedence operator tail
;;

(* Make sure the graph is always sorted! *)
let rec add_operator (state:graph) (operator:Syntax.operator):graph = 
  match state with
  | [] -> [(operator.prec, update_precedence operator [])]
  | (prec, lst) :: tail -> 
    if prec = operator.prec then
      (prec, update_precedence operator lst) :: tail
    else
      if prec < operator.prec then
        (prec, lst) :: add_operator tail operator
      else
        (operator.prec, update_precedence operator []) :: state
