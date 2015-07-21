(** Evaluation rules, small-step operational semantics.

   This module is for demonstration purposes only. It is inefficient
   and not used by the toplevel, which compiles programs to "machine"
   language, see modules Machine and Compile.
*)

open Syntax

(** [is_value e] returns true, if program [e] is a value. *)
let is_value = function
  | Int _ | Bool _ | Fun _ | Abort -> true
  | Var _ | Times _ | Division _ | Plus _ | Minus _
  | Equal _ | Less _ | If _ | Apply _ -> false

(** An exception indicating a value. *)
exception Value

(** An exception indicating a runtime error. *)
exception Runtime

(** [eval1 e] performs a single evaluation step. It raises exception
    Value if [e] is a value. *)
let rec eval1 = function
  | Var _ -> raise Runtime
  | Int _ | Bool _ | Fun _ -> raise Value
  | Division (Int k1, Int k2) -> Int (k1 / k2)
  | Division (Int k1, Abort)  -> Abort
  | Division (Int k1, e2)  -> Division (Int k1, eval1 e2)
  | Division (Abort, e2)   -> Abort
  | Division (e1, e2)      -> Division (eval1 e1, e2)
  | Times (Int k1, Int k2) -> Int (k1 * k2)
  | Times (Int k1, Abort)  -> Abort
  | Times (Int k1, e2)     -> Times (Int k1, eval1 e2)
  | Times (Abort, e2)      -> Abort
  | Times (e1, e2)         -> Times (eval1 e1, e2)
  | Plus (Int k1, Int k2)  -> Int (k1 + k2)
  | Plus (Int k1, Abort)   -> Abort
  | Plus (Int k1, e2)      -> Plus (Int k1, eval1 e2)
  | Plus (Abort, e2)       -> Abort
  | Plus (e1, e2)          -> Plus (eval1 e1, e2)
  | Minus (Int k1, Int k2) -> Int (k1 - k2)
  | Minus (Int k1, Abort)  -> Abort
  | Minus (Int k1, e2)     -> Minus (Int k1, eval1 e2)
  | Minus (Abort, e2)      -> Abort
  | Minus (e1, e2)         -> Minus (eval1 e1, e2)
  | Equal (Int k1, Int k2) -> Bool (k1 = k2)
  | Equal (Int k1, Abort)  -> Abort
  | Equal (Int k1, e2)     -> Equal (Int k1, eval1 e2)
  | Equal (Abort, e2)      -> Abort
  | Equal (e1, e2)         -> Equal (eval1 e1, e2)
  | Less (Int k1, Int k2)  -> Bool (k1 < k2)
  | Less (Int k1, Abort)   -> Abort
  | Less (Int k1, e2)      -> Less (Int k1, eval1 e2)
  | Less (Abort, e2)       -> Abort
  | Less (e1, e2)          -> Less (eval1 e1, e2)
  | If (Bool true, e2, e3) -> e2
  | If (Bool false, e2, e3)-> e3
  | If (Abort, e2, e3)     -> Abort
  | If (e1, e2, e3)        -> If (eval1 e1, e2, e3)
  | Apply (Fun (f, x, _, _, e), Abort) -> Abort
  | Apply (Fun (f, x, _, _, e) as v1, v2) when is_value v2 ->
      subst [(f, v1); (x, v2)] e
  | Apply (Fun _ as v1, e2) -> Apply (v1, eval1 e2)
  | Apply (Abort, e2) -> Abort
  | Apply (e1, e2) -> Apply (eval1 e1, e2)
  | _ -> assert false

(** [eval e] evaluates program [e]. The evaluation returns a value,
    diverges, or raises the [Runtime] exception. *)
let rec eval e =
  let rec loop e = if is_value e then e else loop (eval1 e)
  in
    loop e
