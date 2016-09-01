(* Type inference *)

open Syntax

exception Type_error of string

(** [ty_error msg] reports a type error by raising [Type_error msg]. *)
let type_error msg = raise (Type_error msg)

(** [fresh ()] returns an unused type parameter. *)
let fresh =
  let k = ref 0 in
    fun () -> incr k; TParam !k

(** [refresh t] replaces all parameters appearing in [t] with unused ones. *)
let refresh ty =
  let rec refresh s = function
    | TInt -> TInt, s
    | TBool -> TBool, s
    | TParam k ->
	(try
	   List.assoc k s, s
	 with Not_found -> let t = fresh () in t, (k,t)::s)
    | TArrow (t1, t2) ->
	let u1, s'  = refresh s t1 in
	let u2, s'' = refresh s' t2 in
	  TArrow (u1, u2), s''
    | TTimes (t1, t2) ->
	let u1, s'  = refresh s t1 in
	let u2, s'' = refresh s' t2 in
	  TTimes (u1, u2), s''
    | TList t ->
	let u, s' = refresh s t in
	  TList u, s'
  in
    fst (refresh [] ty)

(** [occurs k t] returns [true] if parameter [TParam k] appears in type [t]. *)
let rec occurs k = function
  | TInt -> false
  | TBool -> false
  | TParam j -> k = j
  | TArrow (t1, t2) -> occurs k t1 || occurs k t2
  | TTimes (t1, t2) -> occurs k t1 || occurs k t2
  | TList t -> occurs k t

(** [solve [(t1,u1); ...; (tn,un)] solves the system of equations
    [t1=u1], ..., [tn=un]. The solution is represented by a list of
    pairs [(k,t)], meaning that [TParam k] equals [t]. A type error is
    raised if there is no solution. The solution found is the most general
    one.
*)
let solve eq =
  let rec solve eq sbst =
    match eq with
      | [] -> sbst
	  
      | (t1, t2) :: eq when t1 = t2 -> solve eq sbst
	  
      | ((TParam k, t) :: eq | (t, TParam k) :: eq) when (not (occurs k t)) ->
	  let ts = tsubst [(k,t)] in
	    solve
	      (List.map (fun (ty1,ty2) -> (ts ty1, ts ty2)) eq)
	      ((k,t)::(List.map (fun (n, u) -> (n, ts u)) sbst))
	      
      | (TTimes (u1,v1), TTimes (u2,v2)) :: eq
      | (TArrow (u1,v1), TArrow (u2,v2)) :: eq ->
	  solve ((u1,u2)::(v1,v2)::eq) sbst
	    
      | (TList t1, TList t2) :: eq -> solve ((t1,t2) :: eq) sbst
	  
      | (t1,t2)::_ ->
	  let u1, u2 = rename2 t1 t2 in
	    type_error ("The types " ^ string_of_type u1 ^ " and " ^
			  string_of_type u2 ^ " are incompatible")
  in
    solve eq []

(** [constraints_of gctx e] infers the type of expression [e] and a set
    of constraints, where [gctx] is global context of values that [e]
    may refer to. *)
let rec constraints_of gctx = 
  let rec cnstr ctx = function
    | Var x ->  
	(try
	   List.assoc x ctx, []
	 with Not_found ->
	   (try
	      (* we call [refresh] here to get let-polymorphism *)
	      refresh (List.assoc x gctx), []
	    with Not_found -> type_error ("Unknown variable " ^ x)))
	  
    | Int _ ->  TInt, []

    | Bool _ -> TBool, []

    | Nil -> TList (fresh ()), []

    | Times (e1, e2)
    | Divide (e1, e2)
    | Mod (e1, e2)
    | Plus (e1, e2)
    | Minus (e1, e2) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	  TInt, (ty1,TInt) :: (ty2,TInt) :: eq1 @ eq2

    | Equal (e1, e2)
    | Less (e1, e2) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	  TBool, (ty1,TInt) :: (ty2,TInt) :: eq1 @ eq2

    | Cons (e1, e2) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	let ty = TList ty1 in
	  ty, (ty2, ty) :: eq1 @ eq2

    | If (e1, e2, e3) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	let ty3, eq3 = cnstr ctx e3 in
	  ty2, (ty1, TBool) :: (ty2, ty3) :: eq1 @ eq2 @ eq3

    | Fun (x, e) ->
	let ty1 = fresh () in
	let ty2, eq = cnstr ((x,ty1)::ctx) e in
	  TArrow (ty1, ty2), eq

    | Rec (x, e) ->
	let ty1 = fresh () in
	let ty2, eq = cnstr ((x,ty1)::ctx) e in
	  ty1, (ty1, ty2) :: eq

    | Match (e1, e2, x, y, e3) ->
	let ty = fresh () in
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	let ty3, eq3 = cnstr ((x,ty)::(y, TList ty)::ctx) e3 in
	  ty2, (ty1, TList ty) :: (ty2, ty3) :: eq1 @ eq2 @ eq3

    | Apply (e1, e2) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	let ty = fresh () in
	  ty, (ty1, TArrow (ty2,ty)) :: eq1 @ eq2

    | Pair (e1, e2) ->
	let ty1, eq1 = cnstr ctx e1 in
	let ty2, eq2 = cnstr ctx e2 in
	  TTimes (ty1, ty2), eq1 @ eq2

    | Fst e ->
	let ty, eq = cnstr ctx e in
	let ty1 = fresh () in
	let ty2 = fresh () in
	  ty1, (ty, TTimes (ty1, ty2)) :: eq

    | Snd e ->
	let ty, eq = cnstr ctx e in
	let ty1 = fresh () in
	let ty2 = fresh () in
	  ty2, (ty, TTimes (ty1, ty2)) :: eq
  in
    cnstr []

(** [type_of ctx e] computes the principal type of expression [e] in
    context [ctx]. *)
let type_of ctx e =
  let ty, eq = constraints_of ctx e in
    tsubst (solve eq) ty
