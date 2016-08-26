(** Desugaring of input syntax to internal syntax. *)

(** [index ~loc x xs] finds the location of [x] in the list [xs]. *)
let index ~loc x =
  let rec index k = function
    | [] -> Zoo.error ~loc "unknown identifier %s" x
    | y :: ys -> if x = y then k else index (k + 1) ys
  in
    index 0

(** [expr xs e] converts an expression of type [Input.expr] to type [Syntax.expr] by
    replacing names in [e] with de Bruijn indices. Here [xs] is the list of names
    currently in scope (i.e., Context.names) *)
let rec expr xs {Zoo.data=e; loc} =
  Zoo.locate ~loc
  (match e with
    | Input.Var x -> Syntax.Var (index ~loc x xs)
    | Input.Lambda (x, e) -> Syntax.Lambda (x, expr (x :: xs) e)
    | Input.App (e1, e2) -> Syntax.App (expr xs e1, expr xs e2))
