open Syntax
module T = Type

let fail fmt =
  Zoo.error ~kind:"Type error" fmt

let new_var level = T.Var (ref (T.Unbound(Name.create (), level)))
let new_gen_var () = T.GenericVar(Name.create ())

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f : T.t -> _ = function
    | T.Var {contents = T.Link ty} -> f ty
    | T.GenericVar _ -> assert false
    | T.Var ({contents = T.Unbound(other_id, other_level)} as other_tvar) ->
      if other_id = tvar_id then
        fail "Recursive types"
      else
      other_tvar := Unbound(other_id, min tvar_level other_level)
    | T.App(ty, ty_arg) ->
      f ty ;
      f ty_arg
    | T.Arrow(param_ty, return_ty) ->
      f param_ty ;
      f return_ty
    | T.Const _ -> ()
  in
  f ty


let rec unify
  = fun ty1 ty2 -> match ty1, ty2 with
  | _, _ when ty1 == ty2 -> ()

  | T.Const name1, T.Const name2 when Syntax.Name.equal name1 name2 -> ()

  | T.App(ty1, ty_arg1), T.App(ty2, ty_arg2) ->
    unify ty1 ty2 ;
    unify ty_arg1 ty_arg2

  | T.Arrow(param_ty1, return_ty1), T.Arrow(param_ty2, return_ty2) ->
    unify param_ty1 param_ty2 ;
    unify return_ty1 return_ty2

  | T.Var {contents = Link ty1}, ty2 -> unify ty1 ty2
  | ty1, T.Var {contents = Link ty2} -> unify ty1 ty2

  | T.Var {contents = Unbound(id1, _)},
    T.Var {contents = Unbound(id2, _)} when id1 = id2 ->
    (* There is only a single instance of a particular type variable. *)
    assert false

  | T.Var ({contents = Unbound(id, level)} as tvar), ty
  | ty, T.Var ({contents = Unbound(id, level)} as tvar) ->
    occurs_check_adjust_levels id level ty ;
    tvar := Link ty

  | _, _ ->
    fail "Cannot unify types %a and %a@." Printer.typ ty1 Printer.typ ty2



let rec generalize level = function
  | T.Var {contents = Unbound(id, other_level)} when other_level > level ->
    T.GenericVar id
  | T.App(ty, ty_arg) ->
    App(generalize level ty, generalize level ty_arg)
  | T.Arrow(param_ty, return_ty) ->
    Arrow(generalize level param_ty, generalize level return_ty)

  | T.Var {contents = Link ty} -> generalize level ty

  | ( T.GenericVar _
    | T.Var {contents = Unbound _}
    | T.Const _
    ) as ty -> ty

(** The real generalization function that is aware of the value restriction. *)
let generalize level ty exp =
  if Syntax.is_value exp then
    generalize level ty
  else
    ty

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f = function
    | T.Const _ as ty -> ty
    | T.Var {contents = Link ty} -> f ty
    | T.GenericVar id ->
      begin try
          Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var ;
          var
      end
    | T.Var {contents = Unbound _} as ty -> ty
    | T.App(ty, ty_arg) ->
      App(f ty, f ty_arg)
    | T.Arrow(param_ty, return_ty) ->
      Arrow(f param_ty, f return_ty)
  in
  f ty

let constant = let open T in function
  | Int _ -> int
  | Plus  -> int @-> int @-> int
  | NewRef ->
    let a = new_gen_var () in
    a @-> T.App (ref, a)
  | Get ->
    let a = new_gen_var () in
    T.App (ref, a) @-> a
  | Set ->
    let a = new_gen_var () in
    T.App (ref, a) @-> a @-> a
      
let rec infer_value env level = function
  | Constant c -> instantiate level @@ constant c
  | Y -> instantiate level @@ T.new_y ()
  | Lambda(param, body_expr) ->
    let param_ty = new_var level in
    let fn_env = T.Env.add param param_ty env in
    let return_ty = infer fn_env level body_expr in
    T.Arrow (param_ty, return_ty)
  | Ref v ->
    T.App (T.ref, infer_value env level !v)

and infer env level = function
  | V v -> infer_value env level v
  | Var name ->
    instantiate level @@ T.Env.find name env
  | Let(var_name, value_expr, body_expr) ->
    let var_ty = infer env (level + 1) value_expr in
    let generalized_ty = generalize level var_ty value_expr in
    infer (T.Env.add var_name generalized_ty env) level body_expr
  | App(fn_expr, arg) ->
    let f_ty = infer env level fn_expr in
    infer_app env level f_ty arg

and infer_app env level f_ty = function
  | [] -> f_ty
  | e::t ->
    let param_ty = infer env level e in
    let return_ty = new_var level in
    unify f_ty (T.Arrow (param_ty, return_ty)) ;
    infer_app env level return_ty t

let infer_top env e =
  let ty = infer env 1 e in
  let ty = generalize 0 ty e in
  ty
