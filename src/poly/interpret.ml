(** An efficient interpreter. *)

open Syntax

type environment = (name * value ref) list

and value =
  | VInt of int
  | VBool of bool
  | VNil
  | VClosure of environment * expr

exception Runtime_error of string

let runtime_error msg = raise (Runtime_error msg)

let rec interp env = function
  | Var x ->
      (try
	 let r = List.assoc x env in
	   match !r with
	       VClosure (env', e) -> let v = interp env' e in r := v ; v
	     | v -> v
       with
	   Not_found -> runtime_error ("Unknown variable " ^ x))
  | Int k -> VInt k
  | Bool b -> VBool b
  | Times (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 * k2)
	 | _ -> runtime_error "Integers expected in multiplication")
  | Divide (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt 0  -> runtime_error ("Division by 0")
	 | VInt k1, VInt k2 -> VInt (k1 / k2)
	 | _ -> runtime_error "Integers expected in division")
  | Mod (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt 0  -> runtime_error ("Division by 0")
	 | VInt k1, VInt k2 -> VInt (k1 mod k2)
	 | _ -> runtime_error "Integers expected in remainder")
  | Plus (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 + k2)
	 | _ -> runtime_error "Integers expected in addition")
  | Minus (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 - k2)
	 | _ -> runtime_error "Integers expected in subtraction")
  | Equal (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VBool (k1 = k2)
	 | _ -> runtime_error "Integers expected in =")
  | Less (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VBool (k1 < k2)
	 | _ -> runtime_error "Integers expected in <")
  | If (e1, e2, e3) ->
      (match interp env e1 with
	 | VBool true -> interp env e2
	 | VBool false -> interp env e3
	 | _ -> runtime_error "Boolean expected in if")
  | Fun _ as e -> VClosure (env, e)
  | Apply (e1, e2) ->
      (match interp env e1 with
	 | VClosure (env', Fun (x, e)) ->
	     interp ((x, ref (VClosure (env, e2)))::env') e
	 | _ -> runtime_error "Function expected in application")
  | Pair _ as e ->  VClosure (env, e)
  | Fst e ->
      (match interp env e with
	 | VClosure (env', Pair (e1, e2)) -> interp env' e1
	 | _ -> runtime_error "Pair expected in fst")
  | Snd e ->
      (match interp env e with
	 | VClosure (env', Pair (e1, e2)) -> interp env' e2
	 | _ -> runtime_error "Pair expected in snd")
  | Rec (x, e) -> 
      let rec env' = (x,ref (VClosure (env',e))) :: env in
	interp env' e
  | Nil -> VNil
  | Cons _ as e -> VClosure (env, e)
  | Match (e1, e2, x, y, e3) ->
      (match interp env e1 with
	 | VNil -> interp env e2
	 | VClosure (env', Cons (d1, d2)) ->
	     interp ((x,ref (VClosure(env',d1)))::(y,ref (VClosure(env',d2)))::env) e3
	 | _ -> runtime_error "List expected in match")


(** [print_result n v] prints at most [n] nodes of the value [v]. *)
let rec print_result n v =
  (if n = 0 then
     print_string "..."
   else
     match v with
       | VInt k -> print_int k
       | VBool b -> print_string (string_of_bool b)
       | VNil -> print_string "[]"
       | VClosure (env, Pair (e1, e2)) ->
	   print_char '(' ;
	   print_result (n/2) (interp env e1) ;
	   print_string ", " ;
	   print_result (n/2) (interp env e2) ;
	   print_char ')'
       | VClosure (env, Cons (e1, e2)) ->
	   let v1 = interp env e1 in
	     (match v1 with
		  VClosure (_, Cons _) ->
		    print_char '(' ; print_result (n/2) v1 ; print_char ')'
		| _ -> print_result (n/2) v1) ;
	     print_string " :: " ;
	     print_result (n-1) (interp env e2)
       | VClosure (_, Fun _) -> print_string "<fun>"
       | _ -> print_string "?"
  ) ;
  flush stdout
