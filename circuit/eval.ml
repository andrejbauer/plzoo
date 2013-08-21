(** Evaluation of circuits. *)

module S = Syntax

type value =
  | Int of int
  | Bool of bool
  | String of string

type environment = {
  funcs : (S.name * (value list -> value)) list ;
  vars : (S.name * value) list
}

let empty_env = { funcs = []; vars = [] }

let extend_var env x v = { funcs = env.funcs ; vars = (x,v) :: env.vars }

let extend_func env f v = { funcs = (f, v) :: env.funcs ; vars = env.vars }

let string_of_value = function
  | Int k -> string_of_int k
  | Bool b -> string_of_bool b
  | String s -> s

exception Runtime of string

let runtime_error msg = raise (Runtime msg)

let to_int = function
  | Int k -> k
  | Bool _ -> runtime_error "Integer expected"
  | String _ -> runtime_error "Integer expected"

(* [eval funcs e] evaluates expression [e] where [funcs] contains the definitions
   of functions. *)
let rec eval env = function
  | S.Var x -> 
    (try
       List.assoc x env.vars
     with Not_found -> runtime_error ("Unkonwn variable " ^ x))
  | S.Int k -> Int k
  | S.Bool b -> Bool b
  | S.String s -> String s
  | S.Apply (f, lst) ->
    (try
       let lst = List.map (eval env) lst in
       let f = List.assoc f env.funcs in
       f lst
     with Not_found -> runtime_error ("Unknown function " ^ f))
  | S.Cond (e1, e2, e3) ->
    (match eval env e1 with
      | Bool true -> eval env e2
      | Bool false -> eval env e3
      | Int _ -> runtime_error "Boolean expected"
      | String _ -> runtime_error "Boolean expected")
  | S.Let (x, e1, e2) ->
    let v = eval env e1 in
    eval (extend_var env x v) e2
  | S.Copy (e1, x, y, e2) ->
    let v = eval env e1 in
    let env = extend_var env x v in
    let env = extend_var env y v in
    eval env e2
      
let compile_box env xs e =
  fun vs ->
    if List.length xs <> List.length vs
    then
      runtime_error (string_of_int (List.length xs) ^ " arguments expected but got " ^ string_of_int (List.length vs))
    else
      let env = List.fold_right (fun (x,v) env -> extend_var env x v) (List.combine xs vs) env in
      eval env e
