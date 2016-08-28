(** Evaluation of expressions *)

(** Expressions evaluate to objects which are represented by the type [ob]. *)
type value =
  {
    as_int : int option ; (** as integer *)
    as_bool : bool option ; (** as boolean *)
    as_func : closure option ; (** as function **)
    fields : (Syntax.name * value ref) list (** as object *)
  }

(** A closure [(th, (x, env, e))] represents a function [fun x -> e] in
    environment [th, env], where [th] is the value of object [this] and [env]
    is the environment of local definitions accessible by the function. *)
and closure = value option * (Syntax.name * env * Syntax.expr)

(** An environment is a list of pairs [(x,ob)], mapping a variable [x] to a [value]. *)
and env = (Syntax.name * value) list

let unit_obj = {as_int=None; as_bool=None; as_func=None; fields=[]}

let mk_int i = {as_int=Some i; as_bool=None; as_func=None; fields=[]}

let mk_bool b = {as_int=None; as_bool=Some b; as_func=None; fields=[]}

let mk_func f = {as_int=None; as_bool=None; as_func=Some f; fields=[]}

let mk_obj lst = {as_int=None; as_bool=None; as_func=None; fields=lst}

(** [copy ob] makes a shallow copy of object [ob]. *)
let rec copy ob =
  { ob with fields = List.map (fun (x,v) -> (x, ref (!v))) ob.fields }

let get_int = function
  | {as_int = Some i; _} -> i
  | _ -> Zoo.error "integer expected"

let get_bool = function
  | {as_bool = Some b; _} -> b
  | _ -> Zoo.error "boolean expected"

let get_func = function
  | {as_func = Some f; _} -> f
  | _ -> Zoo.error "function expected"

(** [get_attr x ob] returns the value of attribute [x] in object [ob]. *)
let rec get_attr x {fields=lst} =
  try
    List.assoc x lst
  with
    Not_found -> Zoo.error "no such field %s" x

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

(** [print_obj ob ppf] pretty-prints the given object. *)
let rec print_value { as_int=i; as_bool=b; as_func=f; fields=lst } ppf =
  let fs =
    (match i with None -> [] | Some i -> [fun ppf -> Format.fprintf ppf "%d" i]) @
    (match b with None -> [] | Some b -> [fun ppf -> Format.fprintf ppf "%b" b]) @
    (match f with None -> [] | Some _ -> [fun ppf -> Format.fprintf ppf "<fun>"])
  and g ppf =
    Format.fprintf ppf "{@[" ;
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
      (fun ppf (x, v)-> Format.fprintf ppf "%s=@[<hov>%t@]" x (print_value !v))
      ppf
      lst ;
    Format.fprintf ppf "@]}"
  in
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf " with ")
    (fun ppf f -> f ppf)
    ppf
    (match fs, lst with
     | _::_, [] -> fs
     | _, _ -> fs @ [g])

(** [eval env e] evaluates expression [e] in environment [env].
    It returns a value of type [ob]. *)
let eval env e = 
  let rec eval th env = function

    | Syntax.Var x ->
       (try List.assoc x env with Not_found -> Zoo.error "no such variable %s" x)

    | Syntax.Int k -> mk_int k

    | Syntax.Bool b -> mk_bool b

    | Syntax.ArithOp (op, e1, e2) -> 
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       let v = arith op (get_int v1) (get_int v2) in
       mk_int v

    | Syntax.Not e ->
       let v = eval th env e in
       mk_bool (not (get_bool v))

    | Syntax.CmpOp (op, e1, e2) -> 
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       mk_bool (cmp op (get_int v1) (get_int v2))

    | Syntax.BoolOp (Syntax.And, e1, e2) ->
       mk_bool (get_bool (eval th env e1) && get_bool (eval th env e2))

    | Syntax.BoolOp (Syntax.Or, e1, e2) ->
       mk_bool (get_bool (eval th env e1) || get_bool (eval th env e2))

    | Syntax.If (e1, e2, e3) ->
       if get_bool (eval th env e1) then
	 eval th env e2
       else
	 eval th env e3

    | Syntax.Skip -> unit_obj

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

    | Syntax.Fun (x, e) -> mk_func (th, (x, env, e))

    | Syntax.This ->
       (match th with
	| Some v -> v
	| None -> Zoo.error "invalid use of 'this'")

    | Syntax.Object lst ->
       mk_obj (List.map (fun (x,e) -> (x, ref (eval th env e))) lst)

    | Syntax.Copy e -> copy (eval th env e)

    | Syntax.With (e1, e2) ->
       let join x y = match x, y with  _, ((Some _) as y) -> y | x, _ -> x in
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       { as_int = join v1.as_int v2.as_int ;
         as_bool = join v1.as_bool v2.as_bool ;
         as_func = join v1.as_func v2.as_func ;
         fields = (List.fold_left 
                     (fun lst (x,v) -> if not (List.mem_assoc x lst) then (x,v) :: lst else lst)
                     v2.fields v1.fields)
       }

    | Syntax.Project (e, x) ->
       let u = eval th env e in
       let v = !(get_attr x u) in
       (match v.as_func with
        | None -> v
        | Some (_, c) -> { v with as_func = Some (Some u, c) })

    | Syntax.Assign (e1, x, e2) ->
       let v1 = eval th env e1 in
       let v2 = eval th env e2 in
       (get_attr x v1) := v2; v2
  in
  eval None env e
