(** Normalization of expressions. *)

open Syntax
open Context

let lookup k env =
  match List.nth env k with
    | Some e -> Some (Syntax.shift (k+1) e)
    | None -> None

let extend env = None :: env

(** [norm env e] evaluates expression [e] in environment [env].
    The optional arguments [~eager] and [~deep] tell whether arguments
    should be evaluated eagerly and whether to evaulate deep abstractions. *)
let norm ?(eager=false) ?(deep=false) =
  let rec norm env ({Zoo.data=e'; loc} as e) =
    match e' with

      | Var k ->
        (match lookup k env with
          | None -> e
          | Some e -> norm env e)

      | Subst (s, e') ->
        norm env (subst s e')

      | Lambda (x, e') -> 
        if deep
        then
          let e' = norm (extend env) e' in
            mk_lambda x e'
        else e

      | App (e1, e2) ->
        let {Zoo.data=e1'} as e1 = norm env e1 in
          (match e1' with
            | Lambda (x, e) -> norm env (mk_subst (Dot (e2, idsubst)) e)
            | Var _ | App _ -> 
              let e2 = (if eager then norm env e2 else e2) in 
                Zoo.locate ~loc (App (e1, e2))
            | Subst _ ->
              Zoo.error ~loc:(e2.Zoo.loc) "function expected")
  in
    norm
