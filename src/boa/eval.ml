(** Evaluation of expressions *)

open Syntax

(** Exception [Runtime_error] is raised if evaluation gets stuck. *)
exception Runtime_error of string

let runtime msg = raise (Runtime_error msg)

(** [copy ob] makes a shallow copy of object [ob]. *)
let rec copy = function
  | (ObjInt _ | ObjBool _ | ObjFunc _) as u -> u
  | ObjDict lst -> ObjDict (List.map (fun (x,v) -> (x, ref (!v))) lst)
  | ObjWith (u,v) -> ObjWith (copy u, copy v)

(** [attributes ob] returns the list of atributes of object [ob]. *)
let rec attributes = function
  | ObjInt _ | ObjBool _ | ObjFunc _ -> []
  | ObjDict lst -> List.map fst lst
  | ObjWith (u, v) ->
      let lst1 = attributes u in
      let lst2 = attributes v in
	lst1 @ (List.filter (fun x -> not (List.mem x lst1)) lst2)

(** [get_int ob] returns [ob] as an integer. *)
let rec get_int = function
  | ObjInt k -> k
  | ObjBool _ | ObjFunc _ | ObjDict _ -> runtime "Not an integer"
  | ObjWith (u, v) -> (try get_int v with Runtime_error _ -> get_int u)

(** [get_bool ob] returns [ob] as a boolean. *)
let rec get_bool = function
  | ObjBool b -> b
  | ObjInt _ | ObjFunc _ | ObjDict _ -> runtime "Not an boolean"
  | ObjWith (u, v) -> (try get_bool v with Runtime_error _ -> get_bool u)

(** [get_func ob] returns [ob] as a function. *)
let rec get_func = function
  | ObjFunc c -> c
  | ObjInt _ | ObjBool _ | ObjDict _ -> runtime "Not a function"
  | ObjWith (u, v) -> (try get_func v with Runtime_error _ -> get_func u)

(** [get_attr x ob] returns the value of attribute [x] in object [ob]. *)
let rec get_attr x = function
  | ObjInt _ | ObjBool _ | ObjFunc _ -> runtime ("No such attribute " ^ x)
  | ObjDict d -> (try List.assoc x d with Not_found -> runtime ("No such attribute " ^ x))
  | ObjWith (u, v) -> (try get_attr x v with Runtime_error _ -> get_attr x u)

(** Mapping from arithmetical operations to corresponding Ocaml functions. *)
let arith = function
  | Plus ->  ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Divide -> ( / )
  | Remainder -> ( mod )

(** Mapping from comparisons to corresponding Ocaml functions. *)
let cmp = function
  | Equal -> ( = )
  | Unequal -> ( <> )
  | Less -> ( < )


(** [string_of_obj ob] converts [ob] to a string. *)
let rec string_of_obj u =
  let primitives =
    (try [string_of_int (get_int u)] with Runtime_error _ -> []) @
    (try [string_of_bool (get_bool u)] with Runtime_error _ -> []) @
    (try ignore (get_func u); ["<fun>"] with Runtime_error _ -> [])
  in
    String.concat " with " (
      primitives @
	(match attributes u with
	   | [] -> if primitives = [] then ["{}"] else []
	   | lst -> ["{" ^
	       (String.concat ", " 
		  (List.map (fun x -> x ^ " = " ^ string_of_obj !(get_attr x u)) lst)
	       ) ^ "}"]))

(** [eval th env e] evaluates expression [e] in environment [env] with object
    this set to [th]. It returns a value of type [ob]. *)
let rec eval th env = function

  | Var x ->
      (try List.assoc x env with Not_found -> runtime ("No such variable " ^ x))

  | Int k -> ObjInt k

  | Bool b -> ObjBool b

  | ArithOp (op, e1, e2) -> 
      let v1 = eval th env e1 in
      let v2 = eval th env e2 in
	ObjInt (arith op (get_int v1) (get_int v2))

  | Not e ->
      let v = eval th env e in
	ObjBool (not (get_bool v))

  | CmpOp (op, e1, e2) -> 
      let v1 = eval th env e1 in
      let v2 = eval th env e2 in
	ObjBool (cmp op (get_int v1) (get_int v2))

  | BoolOp (And, e1, e2) ->
      ObjBool (get_bool (eval th env e1) && get_bool (eval th env e2))

  | BoolOp (Or, e1, e2) ->
      ObjBool (get_bool (eval th env e1) || get_bool (eval th env e2))

  | If (e1, e2, e3) ->
      if get_bool (eval th env e1) then
	eval th env e2
      else
	eval th env e3

  | Skip -> ObjDict []

  | Seq (e1, e2) ->
      ignore (eval th env e1) ; eval th env e2

  | Let (x, e1, e2) ->
      let v = eval th env e1 in
	eval th ((x,v)::env) e2

  | App (e1, e2) ->
      let v1 = eval th env e1 in
      let v2 = eval th env e2 in
      let th', (x, env', e) = get_func v1 in
	eval th' ((x,v2)::env') e

  | Fun (x, e) -> ObjFunc (th, (x, env, e))

  | This ->
      (match th with
	 | Some v -> v
	 | None -> runtime "No this here")

  | Object lst ->
      ObjDict (List.map (fun (x,e) -> (x, ref (eval th env e))) lst)

  | Copy e -> copy (eval th env e)

  | With (e1, e2) ->
      let v1 = eval th env e1 in
      let v2 = eval th env e2 in
	ObjWith (v1, v2)

  | Project (e, x) ->
      let u = eval th env e in
      let v = !(get_attr x u) in
	(try
	   (* Ce je [e.x] funkcija, ji nastavimo vrednost this na [e]. *)
	   let (_, c) = get_func v in
	     ObjWith (v, ObjFunc (Some u, c))
	 with Runtime_error _ -> v)

  | Assign (e1, x, e2) ->
      let v1 = eval th env e1 in
      let v2 = eval th env e2 in
	(get_attr x v1) := v2; v2
