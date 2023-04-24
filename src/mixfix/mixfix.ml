type associativity = Left | Right | None

type fixity = Prefix | Infix of associativity | Postfix | Closed

type operator = {
    names : string list ;
    fixity : fixity ;
    precedence : int
}
(* Example: { names = ["if"; "then"; "else"] ; fixity = Prefix } *)

type t = operator list

let empty = []

let add_operator xs prec asc p = failwith "add_operator: not implemented"

let rec expr p lst =
  (* here main work happens, we have a list of exprs *)
  failwith "expr: missing cases"

and basic p = function
  | Input.Expr lst -> basic p lst
  | Input.Builtin s -> Syntax.Builtin s
  | Fun (x, ty, e) -> Syntax.Fun (x, ty, expr p e)
  | _ -> failwith "basic: missing cases"

let toplevel_cmd p = function
  | Input.TopExpr e -> Syntax.Expr (expr p e)
  | Input.Def (x, e) -> Syntax.Def (x, expr p e)
  | Input.Mixfix (op, prec, asc) -> Syntax.Mixif (add_operator op prec asc p)
  | Input.Quit -> p, Syntax.Quit
