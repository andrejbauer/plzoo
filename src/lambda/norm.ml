(** Normalization of expressions. *)

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

      | Syntax.Var k ->
        (match lookup k env with
          | None -> e
          | Some e -> norm env e)

      | Syntax.Subst (s, e') ->
        norm env (Syntax.subst s e')

      | Syntax.Lambda (x, e') -> 
        if deep
        then
          let e' = norm (extend env) e' in
            Syntax.mk_lambda x e'
        else e

      | Syntax.App (e1, e2) ->
         let e2 = (if eager then norm env e2 else e2) in 
         let {Zoo.data=e1'} as e1 = norm env e1 in
          (match e1' with
            | Syntax.Lambda (x, e) -> 
               norm env (Syntax.mk_subst (Syntax.Dot (e2, Syntax.idsubst)) e)
            | Syntax.Var _ | Syntax.App _ -> Zoo.locate ~loc (Syntax.App (e1, e2))
            | Syntax.Subst _ ->
               Zoo.error ~loc:(e1.Zoo.loc) "function expected")
  in
    norm
