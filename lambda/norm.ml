(** Normalization of expressions. *)

open Syntax
open Context

let lookup k env =
  match List.nth env k with
    | Some e -> Some (Syntax.shift (k+1) e)
    | None -> None

let extend env = None :: env

let env_of_ctx ctx = List.map (function Parameter _ -> None | Definition (_, e) -> Some e) ctx.decls

(** [norm env e] evaluates expression [e] in environment [env] to a weak head normal form,
    while [norm ~weak:false env e] evaluates to normal form. *)
let norm ?(weak=false) =
  let rec norm env ((e', loc) as e) =
    match e' with

      | Var k ->
        (match lookup k env with
          | None -> e
          | Some e -> norm env e)

      | Subst (s, e') ->
        norm env (subst s e')

      | Id (e1, e2, t) ->
        if weak
        then e
        else
          let e1 = norm env e1 in
          let e2 = norm env e2 in
          let t = norm env t in
            mk_id e1 e2 t

      | Refl e' ->
        if weak
        then e
        else
          let e' = norm env e' in
            mk_refl e'

      | Transport (a, p, e') ->
        let (p', _) as p = norm env p in
          (match p' with
            | Refl _ -> norm env e'
            | _ ->
              let a = if weak then a else norm env a in
              let e' = if weak then e' else norm env e' in
                mk_transport a p e')

      | Nat -> e

      | Zero -> e

      | Succ e' -> if weak then e else mk_succ (norm env e')

      | NatRec (a, x, f, n) ->
        let (n', _) as n = norm env n in
          (match n' with
            | Zero -> norm env x
            | Succ m ->
              let a = if weak then a else norm env a in
              let f = if weak then f else norm env f in
              let x = if weak then x else norm env x in
              let e = mk_app (mk_app f m) (mk_natrec a x f m) in
                if weak then e else norm env e
            | _ ->
              let a = if weak then a else norm env a in
              let f = if weak then f else norm env f in
              let x = if weak then x else norm env x in
                mk_natrec a x f n)
      | Pi (x, t1, t2) ->
        if weak
        then e
        else mk_pi x (norm env t1) (norm (extend env) t2)

      | Lambda (x, t, e') -> 
        if weak
        then e
        else 
          let t = (match t with None -> None | Some t -> Some (norm env t)) in
          let e' = norm (extend env) e' in
            mk_lambda x t e'

      | App (e1, e2) ->
        let (e1', _) as e1 = norm env e1 in
          (match e1' with
            | Lambda (x, _, e) -> norm env (mk_subst (Dot (e2, idsubst)) e)
            | Var _ | App _ -> 
              let e2 = (if weak then e2 else norm env e2) in 
                App (e1, e2), loc
            | Subst _ | Id _ | Refl _ | Transport _ | Nat | Zero | Succ _ | NatRec _ |
                Pi _ | Ascribe _ | Type | Sort | TyWtn _ | EqWtn _ | TyJdg _ | EqJdg _ ->
              Error.runtime ~loc:(snd e2) "function expected")

      | Ascribe (e', _) ->
        norm env e'

      | Type | Sort -> e

      | TyJdg (e', t) ->
        if weak
        then e
        else
          let e' = norm env e' in
          let t = norm env t in
            mk_tyjdg e' t

      | TyWtn (e', t) ->
        if weak
        then e
        else
          let e' = norm env e' in
          let t = norm env t in
            mk_tywtn e' t

      | EqJdg (e1, e2, t) ->
        if weak
        then e
        else
          let e1 = norm env e1 in
          let e2 = norm env e2 in
          let t = norm env t in
            mk_eqjdg e1 e2 t

      | EqWtn (e1, e2, t) ->
        if weak
        then e
        else
          let e1 = norm env e1 in
          let e2 = norm env e2 in
          let t = norm env t in
            mk_eqwtn e1 e2 t
  in
    norm

(** [nf ctx e] computes the normal form of expression [e]. *)
let nf ctx = norm ~weak:false (env_of_ctx ctx)

(** [whnf ctx e] computes the weak head normal form of expression [e]. *)
let whnf ctx = norm ~weak:true (env_of_ctx ctx)

