(** An efficient interpreter. *)

type environment = (Syntax.name * value) list

and value =
  | VInt of int
  | VBool of bool
  | VThunk of environment * Syntax.expr
  | VFun of environment * Syntax.name * Syntax.expr
  | VReturn of value

let runtime_error ~loc msg = Zoo.error ~kind:"Runtime error" ~loc msg

let rec print_value v ppf =
  match v with
  | VInt k -> Format.fprintf ppf "%d" k
  | VBool b -> Format.fprintf ppf "%b" b
  | VThunk _ -> Format.fprintf ppf "<thunk>"
  | VFun _ -> Format.fprintf ppf "<fun>"
  | VReturn v -> Format.fprintf ppf "return (%t)" (print_value v)

let rec comp env e =
  match e.Zoo.data with

  | Syntax.Fun (x, _, e) -> VFun (env, x, e)

  | Syntax.If (e1, e2, e3) ->
      (match expr env e1 with
	 | VBool true -> comp env e2
	 | VBool false -> comp  env e3
	 | _ -> runtime_error ~loc:(e1.Zoo.loc) "boolean expected")

  | Syntax.Apply (e1, e2) ->
      (match comp env e1 with
	 | VFun (env', x, e) -> 
            let v2 = expr env e2 in
            comp ((x,v2)::env') e
	 | _ -> runtime_error ~loc:(e1.Zoo.loc) "function expected")

  | Syntax.Let (x, e1, e2) ->
      let v = expr env e1 in
      comp ((x,v)::env) e2

  | Syntax.Do (x, e1, e2) ->
      (match comp env e1 with
         | VReturn v -> comp ((x,v)::env) e2
         | _ -> runtime_error ~loc:(e1.Zoo.loc) "return expected")

  | Syntax.Return e -> VReturn (expr env e)

  | Syntax.Force e ->
      (match expr env e with
	 | VThunk (env, e) -> comp env e
	 | _ -> runtime_error ~loc:(e.Zoo.loc) "thunk expected in force")

  | Syntax.Rec (x, _, e') -> comp ((x, VThunk (env, e)) :: env) e'

  | Syntax.Var _
  | Syntax.Int _
  | Syntax.Bool _
  | Syntax.Times _
  | Syntax.Plus _
  | Syntax.Minus _
  | Syntax.Equal _
  | Syntax.Less _
  | Syntax.Thunk _ ->
     runtime_error ~loc:(e.Zoo.loc) "computation expected"

and expr env {Zoo.data=e; loc} =
 match e with

  | Syntax.Var x ->
      (try
	 List.assoc x env
       with
	   Not_found -> runtime_error ~loc "unknown variable %s" x)

  | Syntax.Int k -> VInt k

  | Syntax.Bool b -> VBool b

  | Syntax.Thunk e -> VThunk (env, e)

  | Syntax.Times (e1, e2) ->
      (match (expr env e1), (expr env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 * k2)
	 | _ -> runtime_error ~loc "integers expected in multiplication")

  | Syntax.Plus (e1, e2) ->
      (match (expr env e1), (expr env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 + k2)
	 | _ -> runtime_error ~loc "integers expected in addition")

  | Syntax.Minus (e1, e2) ->
      (match (expr env e1), (expr env e2) with
	 | VInt k1, VInt k2 -> VInt (k1 - k2)
	 | _ -> runtime_error ~loc "integers expected in subtraction")

  | Syntax.Equal (e1, e2) ->
      (match (expr env e1), (expr env e2) with
	 | VInt k1, VInt k2 -> VBool (k1 = k2)
	 | _ -> runtime_error ~loc "integers expected in =")
  | Syntax.Less (e1, e2) ->
      (match (expr env e1), (expr env e2) with
	 | VInt k1, VInt k2 -> VBool (k1 < k2)
	 | _ -> runtime_error ~loc "integers expected in <")

  | Syntax.Force _
  | Syntax.Return _
  | Syntax.Do _
  | Syntax.Let _
  | Syntax.If _
  | Syntax.Fun _
  | Syntax.Apply _
  | Syntax.Rec _ ->
     runtime_error ~loc "expression expected"
