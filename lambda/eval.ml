open Syntax

exception Interrupted of expr

let current_expr = ref None

let eager = ref false
let hnf = ref false

let rec eval env e =
  match e with
      Id x  ->
	(try eval env (List.assoc x env) with Not_found -> e)
    | Lam (x,e') -> e
    | App (e1, e2) ->
	(match eval env e1 with
	     Lam (x, e1') -> eval env (subst [(x, (eval env e2))] e1')
	   | e1' -> App (e1', e2))
    | Pair (e1, e2) -> e
    | Fst e -> (match eval env e with
		    Pair (e1, e2) -> eval env e1
		  | e' -> Fst e')
    | Snd e -> (match eval env e with
		    Pair (e1, e2) -> eval env e2
		  | e' -> Snd e')

	  
let rec eval_command env = function
    Definition (x, e) -> (x,e) :: env
  | Expr e -> 
      (try
	 print_endline (string_of_expr (eval env e)) ;
	 env
       with Sys.Break ->
	 raise (Interrupted (match !current_expr with Some e' -> e' | None -> e)))
	
let rec eval_list env = function
    Definition (x, e) :: lst -> eval_list ((x,e) :: env) lst
  | Expr e :: lst ->
      (try
	 print_endline (string_of_expr (eval env e))
       with Sys.Break ->
	 raise (Interrupted (match !current_expr with Some e' -> e' | None -> e))) ;
      eval_list env lst
  | [] -> env
