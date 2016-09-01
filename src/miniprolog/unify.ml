(** Unification functions. *)

open Syntax

(** [NoUnify] is raised when terms cannot be unified. *)
exception NoUnify

(** [unify_terms env t1 t2] unifies terms [t1] and [t2] in the current
    environment [env]. On success it returns the environment extended with
    the result of unification. On failure it raises [NoUnify]. *)
let rec unify_terms env t1 t2 =
  match subst_term env t1, subst_term env t2 with
    | t1, t2 when t1 = t2 -> env
    | (Var y, t) | (t, Var y) ->
	if occurs y t then
	  raise NoUnify
	else
	  (y,t) :: env
    | Const _, _ -> raise NoUnify
    | App (c1, ts1), App (c2, ts2) when c1 = c2 -> unify_lists env ts1 ts2
    | App _, _ -> raise NoUnify

(** [unify_lists env lst1 lst2] unifies two lists of terms in current
    environment [env] and returns a new environment [env'] on success. It
    raises [NoUnify] on failure or if the lists are not of equal length.
*)
and unify_lists env lst1 lst2 =
  try
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env lst1 lst2
  with Invalid_argument _ -> raise NoUnify

(** [unify_atoms env a1 a2] unifies atomic propositions [a1] and [a2]
    in current environment [env] and returns a new environment [env'] on
    success. It raises [NoUnify] on failure. *)
let unify_atoms env (c1,ts1) (c2,ts2) =
  if c1 = c2 then unify_lists env ts1 ts2 else raise NoUnify
