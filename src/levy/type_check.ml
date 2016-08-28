(** Type checking. *)

open Syntax

let type_error ?loc = Zoo.error ~kind:"Typing error" ?loc

let rec is_ctype = function
  | (VInt | VBool | VForget _) -> false
  | CFree ty -> is_vtype ty
  | CArrow (ty1, ty2) -> is_vtype ty1 && is_ctype ty2

and is_vtype = function
  | VInt | VBool -> true
  | VForget ty -> is_ctype ty
  | (CFree _ | CArrow _) -> false

let check_ctype ?loc ty =
  if not (is_ctype ty) then type_error ?loc "%s is not a computation type" (string_of_type ty)

let check_vtype ?loc ty =
  if not (is_vtype ty) then type_error ?loc "%s is not a value type" (string_of_type ty)

(** [check ctx ty e] checks that expression [e] has computation
    type [ty] in context [ctx].  It raises a type error if it does
    not. *)
let rec check ctx ty e =
  let ty' = type_of ctx e in
  if ty' <> ty then
    type_error ~loc:e.Zoo.loc
	    "%s has type %s but is used as if it had type %s" (string_of_expr e) (string_of_type ty') (string_of_type ty)

(** [type_of ctx e] computes the type of expression [e] in context [ctx].
    It raises type error if [e] does not have a type. *)
and type_of ctx e =
  let loc = e.Zoo.loc in
  match e.Zoo.data with
  | Var x ->
      (try 
	 List.assoc x ctx
       with
	   Not_found -> type_error ~loc "unknown identifier %s" x)
  | Int _ -> VInt
  | Bool _ -> VBool
  | Times (e1, e2) -> check ctx VInt e1 ; check ctx VInt e2 ; VInt
  | Plus (e1, e2) -> check ctx VInt e1 ; check ctx VInt e2 ; VInt
  | Minus (e1, e2) -> check ctx VInt e1 ; check ctx VInt e2 ; VInt
  | Equal (e1, e2) -> check ctx VInt e1 ; check ctx VInt e2 ; VBool
  | Less (e1, e2) -> check ctx VInt e1 ; check ctx VInt e2 ; VBool
  | If (e1, e2, e3) ->
      check ctx VBool e1 ;
      let ty = type_of ctx e2 in
	check_ctype ty ; check ctx ty e3 ; ty
  | Fun (x, ty, e) ->
      check_vtype ty ;
      let ty2 = type_of ((x,ty)::ctx) e in
	check_ctype ty2 ; CArrow (ty, ty2)
  | Apply (e1, e2) ->
      (match type_of ctx e1 with
	 | CArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
	 | ty ->
	     type_error ~loc
        "%s is used as a function but its type is %s"
        (string_of_expr e1) (string_of_type ty))
  | To (e1, x, e2) ->
      (match type_of ctx e1 with
	 | CFree ty1 ->
	     check_vtype ty1 ;
	     let ty2 = type_of ((x,ty1)::ctx) e2 in
	       check_ctype ty2 ; ty2
	 | ty -> type_error ~loc
			    "%s is used in sequencing but its type is %s"
          (string_of_expr e1) (string_of_type ty))
  | Let (x, e1, e2) ->
      let ty1 = type_of ctx e1 in
	check_vtype ty1;
	let ty2 = type_of ((x,ty1)::ctx) e2 in
	  check_ctype ty2 ; ty2
  | Return e ->
      let ty = type_of ctx e in
	check_vtype ty ; CFree ty
  | Force e ->
      (match type_of ctx e with
	 | VForget ty -> check_ctype ty ; ty
	 | ty -> type_error ~loc
      "%s is forced but its type is %s"
      (string_of_expr e) (string_of_type ty))
  | Thunk e ->
      let ty = type_of ctx e in
	check_ctype ty ; VForget ty
  | Rec (x, ty, e) ->
      check_ctype ty ;
      check ((x, VForget ty)::ctx) ty e ;
      ty
