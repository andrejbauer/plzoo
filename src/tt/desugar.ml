(** Desugaring of input syntax to internal syntax. *)

(** [index ~loc x xs] finds the location of [x] in the list [xs]. *)
let index ~loc x =
  let rec index k = function
    | [] -> Error.typing ~loc "unknown identifier %s" x
    | y :: ys -> if x = y then k else index (k + 1) ys
  in
    index 0

(** [desugar ctx e] converts an expression of type [Input.expr] to type
    [Syntax.expr] by replacing names in [e] with de Bruijn indices. *)
let desugar ctx =
  let rec desugar xs (e, loc) =
    (match e with
      | Input.Var x -> Syntax.Var (index ~loc x xs)
      | Input.Universe u -> Syntax.Universe u
      | Input.Pi a -> Syntax.Pi (desugar_abstraction xs a)
      | Input.Lambda a -> Syntax.Lambda (desugar_abstraction xs a)
      | Input.App (e1, e2) -> Syntax.App (desugar xs e1, desugar xs e2)),
    loc
  and desugar_abstraction xs (x, t, e) =
    (x, desugar xs t, desugar (x :: xs) e)
  in
    desugar ctx.Context.names
