(** Small step semantics, for demonstration purposes only. *)

open Syntax

(** [is_value e] returns [true], if [e] is a value. *)
let rec is_value = function
    Int _ | Bool _ | Fun _ | Nil _ | Cons _ | Pair _ -> true
  | Var _ | Times _ | Divide _ | Mod _ | Plus _ | Minus _
  | Equal _ | Less _ | If _ | Apply _
  | Match _ | Rec _ | Fst _ | Snd _ -> false

(** Expression [Eval_error] indicates a runtime error. *)
exception Eval_error

(** [eval1 e] performs one evaluation step of program [e]. If there is
    no next step, it raises [Eval_error], which happens if [e] is a value
    or if [e] gets stuck. *)
let rec eval1 = function
  | Var _ | Int _ | Bool _ | Fun _ | Nil _ | Pair _ | Cons _ -> raise Eval_error
  | Times (Int k1, Int k2) -> Int (k1 * k2)
  | Times (Int k1, e2)     -> Times (Int k1, eval1 e2)
  | Times (e1, e2)         -> Times (eval1 e1, e2)
  | Divide (Int k1, Int k2)-> Int (k1 / k2)
  | Divide (Int k1, e2)    -> Divide (Int k1, eval1 e2)
  | Divide (e1, e2)        -> Divide (eval1 e1, e2)
  | Mod (Int k1, Int k2)   -> Int (k1 mod k2)
  | Mod (Int k1, e2)       -> Mod (Int k1, eval1 e2)
  | Mod (e1, e2)           -> Mod (eval1 e1, e2)
  | Plus (Int k1, Int k2)  -> Int (k1 + k2)
  | Plus (Int k1, e2)      -> Plus (Int k1, eval1 e2)
  | Plus (e1, e2)          -> Plus (eval1 e1, e2)
  | Minus (Int k1, Int k2) -> Int (k1 - k2)
  | Minus (Int k1, e2)     -> Minus (Int k1, eval1 e2)
  | Minus (e1, e2)         -> Minus (eval1 e1, e2)
  | Equal (Int k1, Int k2) -> Bool (k1 = k2)
  | Equal (Int k1, e2)     -> Equal (Int k1, eval1 e2)
  | Equal (e1, e2)         -> Equal (eval1 e1, e2)
  | Less (Int k1, Int k2)  -> Bool (k1 < k2)
  | Less (Int k1, e2)      -> Less (Int k1, eval1 e2)
  | Less (e1, e2)          -> Less (eval1 e1, e2)
  | If (Bool true, e2, e3) -> e2
  | If (Bool false, e2, e3)-> e3
  | If (e1, e2, e3)        -> If (eval1 e1, e2, e3)
  | Apply (Fun (x, _, e), e2) -> subst [(x, e2)] e
  | Apply (e1, e2) -> Apply (eval1 e1, e2)
  | Rec (x, _, e') as e -> subst [(x,e)] e'
  | Match (Nil _, _, e, _, _, _) -> e
  | Match (Cons (e1, e2), _, _, x, y, e) -> subst [(x,e1);(y,e2)] e
  | Match (e1, ty, e2, x, y, e3) -> Match (eval1 e1, ty, e2, x, y, e3)
  | Fst (Pair (e1, _)) -> e1
  | Fst e -> Fst (eval1 e)
  | Snd (Pair (_, e2)) -> e2
  | Snd e -> Snd (eval1 e)

(** [eval n e] evaluates program [e]. It raises [Eval_error] if [e] gets stuck.
    It forces up to [n] levels of evaluation in pairs and lists. This function
    is inefficient and is here for demostration purposes only. See module [Interpret]
    for a more efficient version.
*)
let rec eval n e =
  let rec loop = function
    | Pair (e1, e2) -> Pair (eval (n-1) e1, eval (n-1) e2)
    | Cons (e1, e2) -> Cons (eval n e1, if n <= 0 then e2 else eval (n-1) e2)
    | e when is_value e -> e
    | e -> loop (eval1 e)
  in
    loop e
