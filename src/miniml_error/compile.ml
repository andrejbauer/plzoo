(** MiniML+error compiler. *)

open Machine

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile {Zoo.data=e'; _} =
  match e' with
    | Syntax.Abort -> [IErr]
    | Syntax.Var x -> [IVar x]
    | Syntax.Int k -> [IInt k]
    | Syntax.Bool b -> [IBool b]
    | Syntax.Division (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Equal (e1, e2) -> (compile e1) @ (compile e2) @ [IEqual]
    | Syntax.Less (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
    | Syntax.If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
    | Syntax.Fun (f, x, _, _, e) -> [IClosure (f, x, compile e @ [IPopEnv])]
    | Syntax.Apply (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
