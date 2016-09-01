(** Large step evaluation semantics. *)

open Syntax

exception Runtime_error of string

(** [runtime_error msg] reports a runtime error by raising [Runtime_error msg] *)
let runtime_error msg = raise (Runtime_error msg)

(** [lookup_value x env] looks up the value of [x] in environment [env]. *)
let lookup_value x env =
  try List.assoc x env with Not_found -> runtime_error ("unknown variable " ^ x)

(** [eval env e] evaluates expression [e] in environment [env]. *)
let rec eval env = function
  | Var x -> lookup_value x env
  | Int _ as e -> e
  | Plus (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 + k2)
	 | _, _ -> runtime_error "integers expected in addition")
  | Minus (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 - k2)
	 | _, _ -> runtime_error "integers expected in subtraction")
  | Times (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 * k2)
	 | _, _ -> runtime_error "integers expected in multiplication")
  | Divide (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int 0 -> runtime_error "division by zero"
	 | Int k1, Int k2 -> Int (k1 / k2)
	 | _, _ -> runtime_error "integers expeced in quotient")
  | Bool _ as e -> e
  | Equal (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Bool (k1 = k2)
	 | _, _ -> runtime_error "integers expected in equality")
  | Less (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Bool (k1 < k2)
	 | _, _ -> runtime_error "integers expected in comparison")
  | And (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Bool b1, Bool b2 -> Bool (b1 && b2)
	 | _, _ -> runtime_error "boolean values expected in conjunction")
  | Or (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Bool b1, Bool b2 -> Bool (b1 || b2)
	 | _, _ -> runtime_error "boolean values expected in disjunction")
  | Not b ->
      (match eval env b with
	   Bool b -> Bool (not b)
	 | _ -> runtime_error "boolean values expected in negation")
  | If (e1, e2, e3) ->
      (match eval env e1 with
	   Bool true -> eval env e2
	 | Bool false -> eval env e3
	 | _ -> runtime_error "boolean value expected in conditional")
  | Fun (f, x, _, _, e) ->
      let rec c = Closure ((f,c)::env, x, e) in c
  | Closure _ as e -> e
  | Let (x, e1, e2) -> eval ((x, eval env e1)::env) e2
  | App (e1, e2) ->
      (match eval env e1 with
	   Closure (env', x, e) -> eval ((x,eval env e2)::env') e
	 | _ -> runtime_error "invalid application")
  | Record rs ->
      Record (List.map (fun (l,e) -> (l, eval env e)) rs)
  | Project (e, l) ->
      (match eval env e with
	   Record vs -> eval env (snd (List.find (fun (l',_) -> l = l') vs))
	 | _ -> runtime_error "record expected")

