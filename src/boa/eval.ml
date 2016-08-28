(** Evaluation of expressions *)

(** Expressions evaluate to objects which are represented by the type [ob]. *)
type ob =
  | Int of int                           (** integer *)
  | Bool of bool                         (** boolean *)
  | Func of closure                      (** closure (represents a function) *)
  | Obj of (Syntax.name * ob ref) list   (** object [{a1=e1, ..., an=en}] *)
  | With of ob * ob                      (** extended object [ob1 with ob2] *)

(** A closure [(th, (x, env, e))] represents a function [fun x -> e] in
    environment [th, env], where [th] is the value of object [this] and [env]
    is the environment of local definitions accessible by the function. *)
and closure = ob option * (Syntax.name * env * Syntax.expr)

(** An environment is a list of pairs [(x,ob)], mapping a variable [x] to
    a value [ob]. *)
and env = (Syntax.name * ob) list

(** [copy ob] makes a shallow copy of object [ob]. *)
let rec copy = function
  | (Int _ | Bool _ | Func _) as u -> u
  | Obj lst -> Obj (List.map (fun (x,v) -> (x, ref (!v))) lst)
  | With (u,v) -> With (copy u, copy v)

(** [attributes ob] returns the list of atributes of object [ob]. *)
let rec attributes = function
  | Int _ | Bool _ | Func _ -> []
  | Obj lst -> List.map fst lst
  | With (u, v) ->
      let lst1 = attributes u in
      let lst2 = attributes v in
	lst1 @ (List.filter (fun x -> not (List.mem x lst1)) lst2)

(** Raised when a getter fails *)
exception InvalidGet of string

(** Raise when the object does not have the required field *)
exception InvalidAttr of string

(** [get_int ob] returns [ob] as an integer. *)
let rec get_int = function
  | Int k -> k
  | Bool _ | Func _ | Obj _ -> raise (InvalidGet "integer")
  | With (u, v) -> (try get_int v with InvalidGet _ -> get_int u)

(** [get_bool ob] returns [ob] as a boolean. *)
let rec get_bool = function
  | Bool b -> b
  | Int _ | Func _ | Obj _ -> raise (InvalidGet "boolean")
  | With (u, v) -> (try get_bool v with InvalidGet _ -> get_bool u)

(** [get_func ob] returns [ob] as a function. *)
let rec get_func = function
  | Func c -> c
  | Int _ | Bool _ | Obj _ -> raise (InvalidGet "function")
  | With (u, v) -> (try get_func v with InvalidGet _ -> get_func u)

(** [get_attr x ob] returns the value of attribute [x] in object [ob]. *)
let rec get_attr x = function
  | Int _ | Bool _ | Func _ -> raise (InvalidAttr x)
  | Obj d -> (try List.assoc x d with Not_found -> raise (InvalidAttr x))
  | With (u, v) -> (try get_attr x v with InvalidAttr _ -> get_attr x u)

(** Mapping from arithmetical operations to corresponding Ocaml functions. *)
let arith = function
  | Syntax.Plus ->  ( + )
  | Syntax.Minus -> ( - )
  | Syntax.Times -> ( * )
  | Syntax.Divide -> ( / )
  | Syntax.Remainder -> ( mod )

(** Mapping from comparisons to corresponding Ocaml functions. *)
let cmp = function
  | Syntax.Equal -> ( = )
  | Syntax.Unequal -> ( <> )
  | Syntax.Less -> ( < )

(** [string_of_obj ob] converts [ob] to a string. *)
let rec string_of_obj u =
  let primitives =
    (try [string_of_int (get_int u)] with InvalidGet _ -> []) @
    (try [string_of_bool (get_bool u)] with InvalidGet _ -> []) @
    (try ignore (get_func u); ["<fun>"] with InvalidGet _ -> [])
  in
    String.concat " with " (
      primitives @
	(match attributes u with
	   | [] -> if primitives = [] then ["{}"] else []
	   | lst -> ["{" ^
	       (String.concat ", " 
		  (List.map (fun x -> x ^ " = " ^ string_of_obj !(get_attr x u)) lst)
	       ) ^ "}"]))

(** [eval env e] evaluates expression [e] in environment [env].
    It returns a value of type [ob]. *)
let eval env e = 
  let rec eval th env = function

    | Syntax.Var x ->
       (try List.assoc x env with Not_found -> Zoo.error "no such variable %s" x)

    | Syntax.Int k -> Int k

    | Syntax.Bool b -> Bool b

    | Syntax.ArithOp (op, e1, e2) -> 
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       Int (arith op (get_int v1) (get_int v2))

    | Syntax.Not e ->
       let v = eval th env e in
       Bool (not (get_bool v))

    | Syntax.CmpOp (op, e1, e2) -> 
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       Bool (cmp op (get_int v1) (get_int v2))

    | Syntax.BoolOp (Syntax.And, e1, e2) ->
       Bool (get_bool (eval th env e1) && get_bool (eval th env e2))

    | Syntax.BoolOp (Syntax.Or, e1, e2) ->
       Bool (get_bool (eval th env e1) || get_bool (eval th env e2))

    | Syntax.If (e1, e2, e3) ->
       if get_bool (eval th env e1) then
	 eval th env e2
       else
	 eval th env e3

    | Syntax.Skip -> Obj []

    | Syntax.Seq (e1, e2) ->
       ignore (eval th env e1) ; eval th env e2

    | Syntax.Let (x, e1, e2) ->
       let v = eval th env e1 in
       eval th ((x,v)::env) e2

    | Syntax.App (e1, e2) ->
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       let th', (x, env', e) = get_func v1 in
       eval th' ((x,v2)::env') e

    | Syntax.Fun (x, e) -> Func (th, (x, env, e))

    | Syntax.This ->
       (match th with
	| Some v -> v
	| None -> Zoo.error "invalid use of 'this'")

    | Syntax.Object lst ->
       Obj (List.map (fun (x,e) -> (x, ref (eval th env e))) lst)

    | Syntax.Copy e -> copy (eval th env e)

    | Syntax.With (e1, e2) ->
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       With (v1, v2)

    | Syntax.Project (e, x) ->
       let u = eval th env e in
       let v = !(get_attr x u) in
       (try
	   (* If [e.x] is a function, we set the value of [this] to [e] *)
	   let (_, c) = get_func v in
	   With (v, Func (Some u, c))
	 with InvalidGet _ -> v)

    | Syntax.Assign (e1, x, e2) ->
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       (get_attr x v1) := v2; v2
  in
  try
    eval None env e
  with
  | InvalidGet t -> Zoo.error "expected a %s" t
  | InvalidAttr x -> Zoo.error "no such attribute %s" x
