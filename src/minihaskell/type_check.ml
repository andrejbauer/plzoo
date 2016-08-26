(** Type checking. *)

(** [ty_error msg] raises exception [Type_error msg]. *)
let type_error msg = Zoo.error ~kind:"Type error" msg

(** [check ctx ty e] checks that expression [e] has type [ty] in context [ctx].
    It raises [Type_error] if it does not. *)
let rec check ctx ty e =
  let ty' = type_of ctx e in
    if ty' <> ty then
      type_error
        "%s has type %s but is used as if it has type %s"
        (Syntax.string_of_expr e)
        (Syntax.string_of_type ty')
        (Syntax.string_of_type ty)

(** [type-of ctx e] computes the type of expression [e] in context [ctx].
    It raises [Type_error] if [e] does not have a type. *)
and type_of ctx = function

  | Syntax.Var x ->
      (try List.assoc x ctx with
	   Not_found -> type_error "unknown identifier %s" x)

  | Syntax.Int _ -> Syntax.TInt

  | Syntax.Bool _ -> Syntax.TBool

  | Syntax.Nil ty -> Syntax.TList ty

  | Syntax.Times (e1, e2) ->
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ;
     Syntax.TInt

  | Syntax.Divide (e1, e2) ->
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ;
     Syntax.TInt
  
  | Syntax.Mod (e1, e2) ->
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ; 
     Syntax.TInt
  
  | Syntax.Plus (e1, e2) -> 
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ;
     Syntax.TInt
  
  | Syntax.Cons (e1, e2) ->
     let ty = type_of ctx e1 in
     check ctx (Syntax.TList ty) e2 ;
     Syntax.TList ty
  
  | Syntax.Minus (e1, e2) ->
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ;
     Syntax.TInt
  
  | Syntax.Equal (e1, e2) ->
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ;
     Syntax.TBool
  
  | Syntax.Less (e1, e2) -> 
     check ctx Syntax.TInt e1 ;
     check ctx Syntax.TInt e2 ; 
     Syntax.TBool
  
  | Syntax.If (e1, e2, e3) ->
      check ctx Syntax.TBool e1 ;
      let ty = type_of ctx e2 in
	check ctx ty e3 ; ty

  | Syntax.Fun (x, ty, e) ->
     Syntax.TArrow (ty, type_of ((x,ty)::ctx) e)

  | Syntax.Rec (x, ty, e) ->
     check ((x,ty)::ctx) ty e ;
     ty

  | Syntax.Match (e1, ty, e2, x, y, e3) ->
      (match type_of ctx e1 with
       | Syntax.TList ty1 ->
	  check ctx (Syntax.TList ty) e1;
	  let ty2 = type_of ctx e2 in
	  check ((x,ty)::(y, Syntax.TList ty)::ctx) ty2 e3 ; ty2
       | ty -> type_error "%s is used as a list but its type is %s"
                          (Syntax.string_of_expr e1)
			  (Syntax.string_of_type ty))

  | Syntax.Apply (e1, e2) ->
     (match type_of ctx e1 with
      | Syntax.TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
      | ty ->
	 type_error "%s is used as a function but its type is %s"
                    Syntax.(string_of_expr e1)
		    (Syntax.string_of_type ty))

  | Syntax.Pair (e1, e2) ->
     Syntax.TTimes (type_of ctx e1, type_of ctx e2)

  | Syntax.Fst e ->
      (match type_of ctx e with
       | Syntax.TTimes (ty1, _) -> ty1
       | ty ->
	  type_error "%s is used as a pair but its type is %s"
                     (Syntax.string_of_expr e)
                     (Syntax.string_of_type ty))

  | Syntax.Snd e ->
      (match type_of ctx e with
       | Syntax.TTimes (_, ty2) -> ty2
       | ty ->
	     type_error "%s is used as a pair but its type is %s"
                        (Syntax.string_of_expr e)
			(Syntax.string_of_type ty))
