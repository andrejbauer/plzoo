(** An efficient interpreter. *)

open Syntax

type environment = (name * runtime) list

and runtime =
  | VInt of int
  | VBool of bool
  | VThunk of environment * expr
  | VFun of environment * name * expr
  | VReturn of runtime

exception Runtime_error of string

let runtime_error msg = raise (Runtime_error ("Runtime error: " ^ msg))


let rec string_of_runtime = function
  | VInt k -> string_of_int k
  | VBool b -> string_of_bool b
  | VThunk _ -> "<thunk>"
  | VFun _ -> "<fun>"
  | VReturn v -> "return " ^ string_of_runtime v

let rec interp env e =
  match e.Zoo.data with
  | Var x ->
      (try
	 List.assoc x env
       with
	   Not_found -> runtime_error ("Unknown variable " ^ x))
  | Int k -> VInt k
  | Bool b -> VBool b
  | Thunk e -> VThunk (env, e)
  | Fun (x, _, e) -> VFun (env, x, e)
  | Times (e1, e2) ->
      (match (interp env e1), (interp env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 * k2)
	 | _ -> runtime_error "Integers expected in multiplication")
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
  | Apply (e1, e2) ->
      (match interp env e1, interp env e2 with
	 | VFun (env, x, e), v2 -> interp ((x,v2)::env) e
	 | _, _ -> runtime_error "Function expected in application")
  | Let (x, e1, e2) ->
      let v = interp env e1 in interp ((x,v)::env) e2
  | To (e1, x, e2) ->
      (match interp env e1 with
         | VReturn v -> interp ((x,v)::env) e2
         | _ -> runtime_error "Return expected in sequencing")
  | Return e -> VReturn (interp env e)
  | Force e ->
      (match interp env e with
	 | VThunk (env, e) -> interp env e
	 | _ -> runtime_error "Thunk expected in force")
  | Rec (x, _, e') -> interp ((x, VThunk (env, e)) :: env) e'
