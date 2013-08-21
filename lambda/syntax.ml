(** Abstract syntax of internal expressions. *)

(** Abstract syntax of expressions, where de Bruijn indices are used to represent
    variables. *)
type term = term' * Common.position
and term' =
  | Var of int
  | Subst of substitution * term
  | Id of term * term * sort
  | Refl of term
  | Transport of term * term * term
  | Nat
  | Zero
  | Succ of term
  | NatRec of term * term * term * term
  | Pi of Common.variable * sort * sort
  | Lambda of Common.variable * sort option * term
  | App of term * term
  | Ascribe of term * sort
  | Type
  | Sort
  | TyWtn of term * sort
  | EqWtn of term * term * sort
  | TyJdg of term * sort
  | EqJdg of term * term * sort

and sort = term

(** Explicit substitutions. *)
and substitution =
  | Shift of int
  | Dot of term * substitution

type operation = operation' * Common.position
and operation' =
  | Inhabit of sort                   (* inhabit a sort *)
  | Infer of term                     (* infer the sort of expression *)
  | HasType of term * sort            (* inhabit a typing judgment *)
  | Equal of term * term * sort       (* inhabit judgmental equality *)

(** Computations. *)
type computation = computation' * Common.position
and computation' = 
  | Return of term
  | Abstraction of Common.variable * sort * computation
  | Operation of operation
  | Handle of computation * handler
  | Let of Common.variable * computation * computation

and handler = (term * term * sort * computation) list

(** Expression constructors wrapped in "nowhere" positions. *)
let mk_var k = Common.nowhere (Var k)
let mk_subst s e = Common.nowhere (Subst (s, e))
let mk_id e1 e2 t = Common.nowhere (Id (e1, e2, t))
let mk_refl e = Common.nowhere (Refl e)
let mk_transport a p e = Common.nowhere (Transport (a, p, e))
let mk_nat = Common.nowhere Nat
let mk_zero = Common.nowhere Zero
let mk_succ e = Common.nowhere (Succ e)
let mk_natrec a x f n = Common.nowhere (NatRec (a, x, f, n))
let mk_pi x t1 t2 = Common.nowhere (Pi (x, t1, t2))
let mk_lambda x t e = Common.nowhere (Lambda (x, t, e))
let mk_app e1 e2 = Common.nowhere (App (e1, e2))
let mk_ascribe e t = Common.nowhere (Ascribe (e, t))
let mk_type = Common.nowhere Type
let mk_kind = Common.nowhere Sort
let mk_eqwtn e1 e2 t = Common.nowhere (EqWtn (e1, e2, t))
let mk_eqjdg e1 e2 t = Common.nowhere (EqJdg (e1, e2, t))
let mk_tywtn e t = Common.nowhere (TyWtn (e, t))
let mk_tyjdg e t = Common.nowhere (TyJdg (e, t))

(** The identity substitution. *)
let idsubst = Shift 0

(** [shift k e] shifts the indices in [e] by [k] places. *)
let shift k e = mk_subst (Shift k) e

let mk_arrow t1 t2 = Common.nowhere (Pi ("_", t1, shift 1 t2))

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
  let rec subst s (e', loc) =
    match s, e' with
      | Shift m, Var k -> Var (k + m), loc
      | Dot (a, s), Var 0 -> a
      | Dot (a, s), Var k -> subst s (Var (k - 1), loc)
      | s, Subst (t, e) -> subst s (subst t e)
      | s, Id (e1, e2, t) ->
        let e1 = mk_subst s e1 in
        let e2 = mk_subst s e2 in
        let t = mk_subst s t in
          Id (e1, e2, t), loc
      | s, Refl e ->
        let e = mk_subst s e in
          Refl e, loc
      | s, Transport (a, p, e) ->
        let a = mk_subst s a in
        let p = mk_subst s p in
        let e = mk_subst s e in
          Transport (a, p, e), loc
      | _, (Nat | Zero) -> e', loc
      | s, Succ e -> Succ (mk_subst s e), loc
      | s, NatRec (a, x, f, n) -> NatRec (mk_subst s a, mk_subst s x, mk_subst s f, mk_subst s n), loc
      | s, Pi (x, t1, t2) ->
        let t1 = mk_subst s t1 in
        let t2 = mk_subst (Dot (mk_var 0, compose (Shift 1) s)) t2 in
          Pi (x, t1, t2), loc
      | s, Lambda (x, t, e) ->
        let t = (match t with None -> None | Some t -> Some (mk_subst s t)) in
        let e = mk_subst (Dot (mk_var 0, compose (Shift 1) s)) e in
          Lambda (x, t, e), loc
      | s, App (e1, e2) -> App (mk_subst s e1, mk_subst s e2), loc
      | s, Ascribe (e, t) -> Ascribe (mk_subst s e, mk_subst s t), loc
      | s, (Type | Sort) -> e', loc
      | s, TyJdg (e, t) ->
        let e = mk_subst s e in
        let t = mk_subst s t in
          TyJdg (e, t), loc
      | s, TyWtn (e, t) ->
        let e = mk_subst s e in
        let t = mk_subst s t in
          TyWtn (e, t), loc
      | s, EqJdg (e1, e2, t) ->
        let e1 = mk_subst s e1 in
        let e2 = mk_subst s e2 in
        let t = mk_subst s t in
          EqJdg (e1, e2, t), loc
      | s, EqWtn (e1, e2, t) ->
        let e1 = mk_subst s e1 in
        let e2 = mk_subst s e2 in
        let t = mk_subst s t in
          EqWtn (e1, e2, t), loc
  in
    subst

(** [occurs k e] returns [true] when variable [Var k] occurs freely in [e]. *)
let rec occurs k (e, _) =
  match e with
    | Var m -> m = k
    | Subst (s, e) -> occurs k (subst s e)
    | Id (e1, e2, t) -> occurs k t || occurs k e1 || occurs k e2
    | Refl e -> occurs k e
    | Transport (a, p, e) -> occurs k a || occurs k p || occurs k e
    | Nat | Zero -> false
    | Succ e -> occurs k e
    | NatRec (a, x, f, e) -> occurs k a || occurs k x || occurs k f || occurs k e
    | Pi (_, t1, t2) -> occurs k t1 || occurs (k + 1) t2
    | Lambda (_, None, e) -> occurs (k + 1) e
    | Lambda (_, Some t, e) -> occurs k t || occurs (k + 1) e
    | App (e1, e2) -> occurs k e1 || occurs k e2
    | Ascribe (e, t) -> occurs k e || occurs k t
    | Type | Sort -> false
    | EqJdg (e1, e2, t) -> occurs k t || occurs k e1 || occurs k e2
    | TyJdg (e, t) -> occurs k t || occurs k e
    | EqWtn (e1, e2, t) -> occurs k t || occurs k e1 || occurs k e2
    | TyWtn (e, t) -> occurs k t || occurs k e

(** Compare two terms using alpha-equivalence only. *)
let alpha_equal =
  let rec equal e1loc e2loc =
    match fst e1loc, fst e2loc with
      | Subst (s, e1), _ -> equal (subst s e1loc) e2loc
      | _, Subst (s, e2) -> equal e1loc (subst s e2loc)
      | Var k, Var m -> k = m
      | Id (e11, e12, t1), Id (e21, e22, t2) -> equal t1 t2 && equal e11 e21 && equal e12 e22
      | Refl e1, Refl e2 -> equal e1 e2
      | Transport (a1, p1, e1), Transport (a2, p2, e2) -> equal a1 a2 && equal p1 p2 && equal e1 e2
      | Nat, Nat -> true
      | Zero, Zero -> true
      | Succ e1, Succ e2 -> equal e1 e2
      | NatRec (a1, x1, f1, n1), NatRec (a2, x2, f2, n2) -> equal a1 a2 && equal x1 x2 && equal f1 f2 && equal n1 n2
      | Pi (_, t1, t2), Pi (_, t1', t2') -> equal t1 t1' && equal t2 t2'
      | Lambda (_, _, e1), Lambda (_, _, e2) -> equal e1 e2
      | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
      | Ascribe (e1, _), _ -> equal e1 e2loc
      | _, Ascribe (e2, _) -> equal e1loc e2
      | Type, Type -> true
      | Sort, Sort -> true
      | TyWtn (e1, t1), TyWtn (e2, t2) -> equal e1 e2 && equal t1 t2
      | EqWtn (e11, e12, t1), EqWtn (e21, e22, t2) -> equal t1 t2 && equal e11 e21 && equal e12 e22
      | TyJdg (e1, t1), TyJdg (e2, t2) -> equal e1 e2 && equal t1 t2
      | EqJdg (e11, e12, t1), EqJdg (e21, e22, t2) -> equal t1 t2 && equal e11 e21 && equal e12 e22
      | (Var _ | Id _ | Refl _ | Transport _ | Nat | Zero | Succ _ | NatRec _ | 
          Pi _ | Lambda _ | App _ | Type | Sort | TyWtn _ | EqWtn _ | TyJdg _ | EqJdg _), _ -> false
  in
    equal

let subst_operation s (op, loc) =
  (match op with
    | Inhabit t -> Inhabit (mk_subst s t)
    | Infer t -> Infer (mk_subst s t)
    | HasType (e, t) -> HasType (mk_subst s e, mk_subst s t)
    | Equal (e1, e2, t) -> Equal (mk_subst s e1, mk_subst s e2, mk_subst s t)),
  loc

let rec subst_computation s =
  let rec subst s (c, loc) =
    match c with
      | Return t -> Return (mk_subst s t), loc
      | Abstraction (x, t, c) ->
        let t = mk_subst s t in
        let c = subst (Dot (mk_var 0, compose (Shift 1) s)) c in
          Abstraction (x, t, c), loc
      | Operation op -> Operation (subst_operation s op), loc
      | Handle (c, h) -> Handle (subst s c, subst_handler s h), loc
      | Let (x, c1, c2) ->
        let c1 = subst s c1 in
        let c2 = subst (Dot (mk_var 0, compose (Shift 1) s)) c2 in
          Let (x, c1, c2), loc
  in
    subst s

and subst_handler s lst = List.map (subst_handler_case s) lst

and subst_handler_case s (e1, e2, t, c) =
  let e1 = mk_subst s e1 in
  let e2 = mk_subst s e2 in
  let t = mk_subst s t in
  let c = subst_computation s c in
    (e1, e2, t, c)

let shift_computation k c = subst_computation (Shift k) c
