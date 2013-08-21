(** Type inference. *)

open Syntax
open Context

type size = Small | Large

let max_size u1 u2 =
  match u1 with
    | Small -> u2
    | Large -> Large

(** [equal_at ctx e1 e2 t] compares expressions [e1] and [e2] at sort [t]. It is assumed
    that [t] is a valid sort. It is also assumed that [e1] and [e2] have sort [t]. *)
let rec equal_at ctx e1 e2 t =
  let t = Norm.whnf ctx t in
    match fst t with
      | Id _ -> equal ctx e1 e2
      | Nat -> equal ctx e1 e2
      | Pi (x, t1, t2) ->
        let e1' = mk_app (shift 1 e1) (mk_var 0) in
        let e2' = mk_app (shift 1 e2) (mk_var 0) in
          equal_at (add_parameter x t1 ctx) e1' e2' t2
      | EqJdg _ -> equal ctx e1 e2
      | TyJdg _ -> equal ctx e1 e2
      | Type | Sort -> equal_sort ctx e1 e2
      | Var _ | App _ -> equal ctx e1 e2
      | Refl _ | Transport _ | Zero | Succ _ | NatRec _ | TyWtn _ | EqWtn _ | Lambda _ | Subst _ | Ascribe _ ->
        Error.runtime ~loc:(snd t) "internal error, compare at non-sort"

and equal ctx e1 e2 =
  let e1 = Norm.whnf ctx e1 in
  let e2 = Norm.whnf ctx e2 in
    (Syntax.alpha_equal e1 e2) ||
    begin match fst e1, fst e2 with

      | Type, Type -> true

      | Sort, Sort -> true

      | Id (e1, e2, t), Id (e1', e2', t') ->
        equal_sort ctx t t' &&
        equal_at ctx e1 e1' t &&
        equal_at ctx e2 e2' t

      | Refl e1, Refl e2 ->
        equal ctx e1 e2

      | Transport (a1, p1, e1), Transport (a2, p2, e2) ->
        equal ctx a1 a2 &&
        equal ctx p1 p2 &&
        equal ctx e1 e2

      | Nat, Nat -> true

      | Zero, Zero -> true

      | Succ e1, Succ e2 ->
        equal ctx e1 e2

      | NatRec (a1, x1, f1, n1), NatRec (a2, x2, f2, n2) ->
        equal_sort ctx a1 a2 &&
        equal ctx x1 x2 &&
        equal ctx f1 f2 &&
        equal_at ctx n1 n2 mk_nat

      | EqJdg (e1, e2, t), EqJdg (e1', e2', t') ->
        equal_sort ctx t t' &&
        equal_at ctx e1 e1' t &&
        equal_at ctx e2 e2' t

      | EqWtn (e1, e2, t), EqWtn (e1', e2', t') ->
        equal_sort ctx t t' &&
        equal_at ctx e1 e1' t &&
        equal_at ctx e2 e2' t

      | TyJdg (e1, t1), TyJdg (e2, t2) ->
        equal_sort ctx t1 t2 &&
        equal_at ctx e1 e2 t1

      | TyWtn (e1, t1), TyWtn (e2, t2) ->
        equal_sort ctx t1 t2 &&
        equal_at ctx e1 e2 t1

      | Pi (x, t1, t2), Pi (_, s1, s2) ->
        equal_sort ctx t1 s1 &&
        equal_sort (add_parameter x t1 ctx) t2 s2

      | Var _, Var _

      | App _, App _ -> None <> equal_spine ctx e1 e2

      | (Var _ | Id _ | Refl _ | Nat | Zero | Succ _ | NatRec _ | Transport _ |
         Pi _ | Lambda _ | App _ | Subst _ |
         Ascribe _ | Type | Sort | EqWtn _ | TyWtn _ | EqJdg _ | TyJdg _), _ -> false
    end

and equal_spine ctx e1 e2 =
  match fst e1, fst e2 with
    | Var k1, Var k2 ->
      if k1 = k2
      then Some (lookup_ty k2 ctx)
      else None
    | App (a1, a2), App (b1, b2) ->
      (match equal_spine ctx a1 b1 with
        | None -> None
        | Some t ->
          (match fst (Norm.whnf ctx t) with
            | Pi (x, u1, u2) ->
              if equal_at ctx a2 b2 u1
              then Some (mk_subst (Dot (a2, idsubst)) u2)
              else None
            | _ -> None))
    | (Var _ | Id _ | Refl _ | Transport _ | Nat | Zero | Succ _ | NatRec _ |
        Pi _ | Lambda _ | App _ |
        Subst _ | Ascribe _ | Type | Sort | EqWtn _ | TyWtn _ | EqJdg _ | TyJdg _), _ -> None

(** [t1] and [t2] must be valid sorts. *)
and equal_sort ctx t1 t2 = equal ctx t1 t2

(** [infer ctx e] infers the sort of expression [e] in context [ctx]. *)
let rec infer ctx (e, loc) =
  match e with

    | Var k -> lookup_ty k ctx

    | Id (e1, e2, t) ->
      ignore (check_sort ctx t) ;
      check ctx e1 t ;
      check ctx e2 t ;
      mk_kind

    | Refl e ->
      let t = infer ctx e in
        (match check_sort ctx t with
          | Large -> Error.typing ~loc:(snd t) "this sort is not a Type"
          | Small -> mk_id e e t)

    | Transport (a, p, e) ->
      let (x, t1, t2) = infer_pi ctx a in
        (match check_sort ctx t1 with
          | Large -> Error.typing ~loc:(snd a) "this product should be indexed by a Type"
          | Small ->            
            if not (equal_sort (add_parameter x t1 ctx) t2 mk_type)
            then Error.typing ~loc:(snd a) "this product should be a type family"
            else
              (match infer ctx p with
                | Id (t, e1, e2), _ ->
                  if not (equal_sort ctx t t1)
                  then Error.typing ~loc:(snd p) "this is an equality term at %t but it should be at %t"
                         (Print.expr ctx.names t) (Print.expr ctx.names t1) ;
                  check ctx e1 t ;
                  check ctx e2 t ;
                  check ctx e (mk_app a e1) ;
                  mk_app a e2
                | t -> Error.typing ~loc:(snd p)
                         "this expression has type %t but it should be an equality term" (Print.expr ctx.names t)
              )
        )

    | Nat -> mk_type

    | Zero -> mk_nat

    | Succ e ->
      check ctx e mk_nat ;
      mk_nat

    | NatRec (a, x, f, n) ->
      let (y, t1, t2) = infer_pi ctx a in
        if not (equal_sort ctx t1 mk_nat) then Error.typing ~loc:(snd a) "this product should be indexed by nat" ;
        check ctx x (mk_app a mk_zero) ;
        check ctx f (mk_pi y mk_nat (mk_arrow (mk_app a (mk_var 0)) (mk_app a (mk_succ (mk_var 1))))) ;
        check ctx n mk_nat ;
        mk_app a n

    | Pi (x, t1, t2) ->
      let u1 = check_sort ctx t1 in
      let u2 = check_sort (add_parameter x t1 ctx) t2 in
        (match max_size u1 u2 with
          | Small -> mk_type
          | Large -> mk_kind)

    | Subst (s, e) -> infer ctx (subst s e)

    | App (e1, e2) ->
      let (x, t1, t2) = infer_pi ctx e1 in
        check ctx e2 t1 ;
        mk_subst (Dot (e2, idsubst)) t2

    | Lambda (x, None, _) -> Error.typing ~loc "cannot infer the sort of %s" x

    | Lambda (x, Some t1, e) ->
      ignore (check_sort ctx t1) ;
      let t2 = infer (add_parameter x t1 ctx) e in
        mk_pi x t1 t2

    | Ascribe (e, t) ->
      check ctx e t ;
      t

    | Type -> mk_kind

    | Sort -> Error.typing ~loc "sorry, you cannot use Sort as a sort"

    | TyWtn (e, t) -> mk_tyjdg e t

    | EqWtn (e1, e2, t) -> mk_eqwtn e1 e2 t

    | EqJdg (e1, e2, t) ->
      ignore (check_sort ctx t) ;
      check ctx e1 t ;
      check ctx e2 t ;
      mk_kind

    | TyJdg (e, t) ->
      ignore (check_sort ctx t) ;
      check ctx e t ;
      mk_kind

and check ctx ((e', loc) as e) t =
  ignore (check_sort ctx t) ;
  match e' with
    | Subst (s, e) -> check ctx (subst s e) t (* XXX avoid rechecking t *)
    | Lambda (x, None, e) ->
      (match fst (Norm.whnf ctx t) with
        | Pi (x, t1, t2) -> check (add_parameter x t1 ctx) e t2
        | _ -> Error.typing ~loc "this expression is a function but should have sort@ %t"
          (Print.expr ctx.names t))
    | Sort -> Error.typing ~loc "Sort does not have sort %t" (Print.expr ctx.names t)
    | Var _ | Lambda (_, Some _, _) | Id _ | Refl _ | Transport _ | 
        Nat | Zero | Succ _ | NatRec _ | Pi _ | App _ |
        Ascribe _ | Type | TyJdg _ | EqJdg _ | TyWtn _ | EqWtn _ ->
      let t' = infer ctx e in
        if not (equal_sort ctx t' t) then
          Error.typing ~loc:(snd e) "this expression has sort %t@ but it should have sort %t"
            (Print.expr ctx.names t') (Print.expr ctx.names t)

(* Returns [Small] if the sort is small and [Large] otherwise. *)
and check_sort ctx (e',loc) =
  match e' with
    | Var k ->
      let t = lookup_ty k ctx in
      (match fst (Norm.whnf ctx t) with
        | Type -> Small
        | Sort -> Large
        | _ -> Error.typing ~loc "this expression has sort %t@ but should be a sort" (Print.expr ctx.names t))
    | Subst (s, e) -> check_sort ctx (subst s e)
    | Lambda _ -> Error.typing ~loc "this expression is a function but should be a sort"
    | Id (e1, e2, t) ->
      ignore (check_sort ctx t) ;
      check ctx e1 t ;
      check ctx e2 t ;
      Small
    | Refl _ -> Error.typing ~loc "a reflexivity term is not a sort sort"
    | Transport _ -> Error.typing ~loc "a transport term is not a sort"
    | Nat -> Small
    | Zero -> Error.typing ~loc "0 is not a sort"
    | Succ _ -> Error.typing ~loc "a successor term but should be a sort"
    | NatRec _ -> Error.typing ~loc "this expression should be a sort"
    | Pi (x, t1, t2) ->
      let u1 = check_sort ctx t1 in
      let u2 = check_sort (add_parameter x t1 ctx) t2 in
        max_size u1 u2
    | App (e1, e2) ->
      let (x, t1, t2) = infer_pi ctx e1 in
        check ctx e2 t1 ;
        check_sort (add_parameter x t1 ctx) t2
    | Ascribe (e1, e2) ->
      check ctx e1 e2 ;
      check_sort ctx e1
    | Type -> Large
    | Sort -> Error.typing ~loc "Sort is not a sort"
    | EqWtn _ -> Error.typing ~loc "this expression is an equality witness but should be a sort"
    | TyWtn _ -> Error.typing ~loc "this expression is a typing witness but should be a sort"
    | EqJdg (e1, e2, t) ->
      ignore (check_sort ctx t) ;
      check ctx e1 t ;
      check ctx e2 t ;
      Large
    | TyJdg (e, t) ->
      ignore (check_sort ctx t)  ;
      check ctx e t ;
      Large

and infer_pi ctx e =
  let t = infer ctx e in
    match fst (Norm.whnf ctx t) with
      | Pi (x, t1, t2) -> (x, t1, t2)
      | Subst _ | Ascribe _ -> assert false
      | Var _ | Id _ | Refl _ | Transport _ | App _ | Type | Sort |
          Nat | Zero | Succ _ | NatRec _ |
          EqJdg _ | TyJdg _ | TyWtn _ | EqWtn _ | Lambda _ ->
        Error.typing ~loc:(snd e) "this expression has sort %t@ but it should be a function" (Print.expr ctx.names t)
