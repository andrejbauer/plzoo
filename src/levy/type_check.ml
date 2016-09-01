(** Type checking. *)

type ctype =
  | CFree of vtype           (** free type [F s] *)
  | CArrow of vtype * ctype  (** Function type [s -> t] *)

and vtype =
  | VInt                     (** integer [int] *)
  | VBool                    (** booleans [bool] *)
  | VForget of ctype         (** thunked type [U t] *)

let type_error ?loc = Zoo.error ~kind:"Typing error" ?loc

let rec print_vtype ?max_level vty ppf =
  match vty with
  | VInt -> Format.fprintf ppf "int"
  | VBool -> Format.fprintf ppf "bool"
  | VForget cty -> 
     Zoo.print_parens ?max_level ~at_level:2 ppf
                      "U@ %t"
                      (print_ctype ~max_level:1 cty)

and print_ctype ?max_level cty ppf =
  match cty with
  | CFree vty ->
     Zoo.print_parens ?max_level ~at_level:2 ppf
                      "F@ %t" 
                      (print_vtype ~max_level:1 vty)
  | CArrow (vty, cty) ->
     Zoo.print_parens ?max_level ~at_level:1 ppf
                      "%t@ ->@ %t"
                      (print_vtype ~max_level:1 vty)
                      (print_ctype ~max_level:2 cty)

let rec as_ctype {Zoo.data=ty; loc} =
  match ty with
  | Syntax.VInt | Syntax.VBool | Syntax.VForget _ ->
     type_error ~loc "this is not a computation type"
  | Syntax.CFree ty -> CFree (as_vtype ty)
  | Syntax.CArrow (ty1, ty2) -> CArrow (as_vtype ty1, as_ctype ty2)

and as_vtype {Zoo.data=ty; loc} =
  match ty with
  | Syntax.VInt -> VInt
  | Syntax.VBool -> VBool
  | Syntax.VForget ty -> VForget (as_ctype ty)
  | Syntax.CFree _ | Syntax.CArrow _ -> 
    type_error ~loc "this is not a value type"

(** [check ctx ty e] checks that expression [e] has computation
    type [ty] in context [ctx].  It raises a type error if it does
    not. *)
let rec check_vtype ctx vty e =
  let vty' = vtype_of ctx e in
  if vty' <> vty then
    type_error ~loc:e.Zoo.loc
               "this expression has value type %t but is used as if its type is %t"
               (print_vtype vty')
               (print_vtype vty)

and check_ctype ctx cty e =
  let cty' = ctype_of ctx e in
  if cty' <> cty then
    type_error ~loc:e.Zoo.loc
               "this expression has computation type %t but is used as if its type is %t"
               (print_ctype cty')
               (print_ctype cty)

(** [vtype_of ctx e] computes the value type of an expression [e] in context [ctx].
    It raises type error if [e] does not have a value type. *)
and vtype_of ctx {Zoo.data=e; loc} =
  match e with
  | Syntax.Var x ->
     (try 
	 List.assoc x ctx
       with
	 Not_found -> type_error ~loc "unknown identifier %s" x)

  | Syntax.Int _ -> VInt

  | Syntax.Bool _ -> VBool

  | Syntax.Times (e1, e2) ->
     check_vtype ctx VInt e1 ;
     check_vtype ctx VInt e2 ;
     VInt

  | Syntax.Plus (e1, e2) ->
     check_vtype ctx VInt e1 ;
     check_vtype ctx VInt e2 ;
     VInt

  | Syntax.Minus (e1, e2) ->
     check_vtype ctx VInt e1 ;
     check_vtype ctx VInt e2 ;
     VInt

  | Syntax.Equal (e1, e2) ->
     check_vtype ctx VInt e1 ;
     check_vtype ctx VInt e2 ;
     VBool

  | Syntax.Less (e1, e2) ->
     check_vtype ctx VInt e1 ;
     check_vtype ctx VInt e2 ;
     VBool

  | Syntax.Thunk e ->
     let ty = ctype_of ctx e in
     VForget ty

  | Syntax.If _
  | Syntax.Fun _
  | Syntax.Apply _
  | Syntax.Do _
  | Syntax.Let _
  | Syntax.Return _
  | Syntax.Force _
  | Syntax.Rec _ ->
     type_error ~loc "a value was expected but a computation was encountered"

(** [ctype_of ctx e] computes the computation type of a computation [e] in context [ctx].
    It raises type error if [e] does not have a computation type. *)
and ctype_of ctx {Zoo.data=e; loc} =
  match e with
  | Syntax.If (e1, e2, e3) ->
     check_vtype ctx VBool e1 ;
     let ty = ctype_of ctx e2 in
     check_ctype ctx ty e3 ;
     ty

  | Syntax.Fun (x, ty, e) ->
     let ty = as_vtype ty in
     let ty2 = ctype_of ((x,ty)::ctx) e in
     CArrow (ty, ty2)

  | Syntax.Apply (e1, e2) ->
     (match ctype_of ctx e1 with
      | CArrow (ty1, ty2) ->
         check_vtype ctx ty1 e2 ;
         ty2
      | ty ->
	 type_error ~loc:(e1.Zoo.loc)
                    "this expression is used as a function but its type is %t"
                    (print_ctype ty))

  | Syntax.Do (x, e1, e2) ->
     (match ctype_of ctx e1 with
      | CFree ty1 -> ctype_of ((x,ty1)::ctx) e2
      | ty -> type_error ~loc:(e1.Zoo.loc)
			 "this expression is sequenced but its type is %t"
                         (print_ctype ty))

  | Syntax.Let (x, e1, e2) ->
     let ty1 = vtype_of ctx e1 in
     ctype_of ((x,ty1)::ctx) e2

  | Syntax.Return e ->
     let ty = vtype_of ctx e in
     CFree ty

  | Syntax.Force e ->
     (match vtype_of ctx e with
      | VForget ty -> ty
      | ty -> type_error ~loc:(e.Zoo.loc)
                         "this expression is forced but its type is %t"
                         (print_vtype ty))

  | Syntax.Rec (x, ty, e) ->
     let ty = as_ctype ty in
     check_ctype ((x, VForget ty)::ctx) ty e ;
     ty

  | Syntax.Var _
  | Syntax.Int _
  | Syntax.Bool _
  | Syntax.Plus _
  | Syntax.Minus _
  | Syntax.Times _
  | Syntax.Equal _
  | Syntax.Less _ 
  | Syntax.Thunk _ ->
     type_error ~loc "a computation was expected but a value was encountered"

