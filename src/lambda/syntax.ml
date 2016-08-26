(** Abstract syntax of internal expressions. *)

(** Abstract syntax of expressions, where de Bruijn indices are used to represent
    variables. *)
type term = term' Zoo.located
and term' =
  | Var of int
  | Subst of substitution * term
  | Lambda of string * term
  | App of term * term

(** Explicit substitutions. *)
and substitution =
  | Shift of int
  | Dot of term * substitution

(** Expression constructors wrapped in "nowhere" positions. *)
let mk_var k = Zoo.locate (Var k)
let mk_subst s e = Zoo.locate (Subst (s, e))
let mk_lambda x e = Zoo.locate (Lambda (x, e))
let mk_app e1 e2 = Zoo.locate (App (e1, e2))

(** The identity substitution. *)
let idsubst = Shift 0

(** [shift k e] shifts the indices in [e] by [k] places. *)
let shift k e = mk_subst (Shift k) e

(** [compose s t] composes explicit subtitutions [s] and [t], i.e.,
    we have [subst (compose s t) e = subst s (subst t e)]. *)
let rec compose s t =
  match s, t with
    | s, Shift 0 -> s
    | Dot (_, s), Shift m -> compose s (Shift (m - 1))
    | Shift m, Shift n -> Shift (m + n)
    | s, Dot (e, s') -> Dot (mk_subst s e, compose s s')

(** [subst s e] applies explicit substitution [s] in expression [e]. It does so
    lazily, i.e., it does just enough to expose the outermost constructor of [e]. *)
let subst =
  let rec subst s {Zoo.data=e'; loc} =
    match s, e' with
      | Shift m, Var k -> Zoo.locate ~loc (Var (k + m))
      | Dot (a, s), Var 0 -> a
      | Dot (a, s), Var k -> subst s (Zoo.locate ~loc (Var (k - 1)))
      | s, Subst (t, e) -> subst s (subst t e)
      | s, Lambda (x, e) ->
        let e = mk_subst (Dot (mk_var 0, compose (Shift 1) s)) e in
          Zoo.locate ~loc (Lambda (x, e))
      | s, App (e1, e2) -> Zoo.locate ~loc (App (mk_subst s e1, mk_subst s e2))
  in
    subst

(** [occurs k e] returns [true] when variable [Var k] occurs freely in [e]. *)
let rec occurs k {Zoo.data=e; _} =
  match e with
    | Var m -> m = k
    | Subst (s, e) -> occurs k (subst s e)
    | Lambda (_, e) -> occurs (k + 1) e
    | App (e1, e2) -> occurs k e1 || occurs k e2

(** Compare two terms using alpha-equivalence only. *)
let alpha_equal =
  let rec equal e1 e2 =
    match e1.Zoo.data, e2.Zoo.data with
      | Subst (s, e1), _ -> equal (subst s e1) e2
      | _, Subst (s, e2) -> equal e1 (subst s e2)
      | Var k, Var m -> k = m
      | Lambda (_, e1), Lambda (_, e2) -> equal e1 e2
      | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
      | (Var _ | Lambda _ | App _), _ -> false
  in
    equal
