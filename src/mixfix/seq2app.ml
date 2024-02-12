

type state = Syntax.operator list

let initial_state = fun _ -> []

let add_operator state operator = operator :: state

let parse_presyntax (env) (presyn_expr) = 
  let rec expr = function
  | Presyntax.Var varname -> Syntax.Var varname
  | Presyntax.Seq es -> (seq_to_app es)
  | Presyntax.Predef x -> Syntax.predef_cascade expr x
    
and seq_to_app (es: Presyntax.expr list): Syntax.expr =
  (* Fold left wants initial value, which we dont really have? *)
  match es with
    | [] -> failwith "seq should be non-empty" 
    | a :: [] -> expr a
    | head :: tail -> Syntax.Apply (expr head, seq_to_app tail)
in 
  expr presyn_expr
;;
