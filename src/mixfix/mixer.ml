(* Transcribing stuff from Presyntax to Syntax  *)

  let determine_fixity (assoc:Presyntax.associativity) (s:string): Syntax.fixity = 
    let first_char = String.get s 0 and last_char = String.get s (String.length s - 1) in
    match (first_char, last_char) with
    | ('_','_') -> (match assoc with
      | Presyntax.NonAssoc -> Syntax.Infix Syntax.NonAssoc
      | Presyntax.LeftAssoc -> Syntax.Infix Syntax.LeftAssoc
      | Presyntax.RightAssoc -> Syntax.Infix Syntax.RightAssoc)
    | ('_',_) -> Syntax.Postfix
    | (_, '_') -> Syntax.Prefix
    | (_, _) -> Syntax.Closed

let create_operator (assoc:Presyntax.associativity) (prec:int) (name:Syntax.name): Syntax.operator=
  let fx = determine_fixity assoc name in 
    let tokens = (List.filter (fun x -> x <> "") (String.split_on_char '_' name)) in
     {tokens ; prec ; fx }


let toplevel_cmd env (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
  match cmd with
  | Presyntax.Expr e -> Syntax.Expr (Parser.parse_presyntax env e)
  | Presyntax.Def (name,  e) -> Syntax.Def (name, Parser.parse_presyntax env e)
  | Presyntax.Mixfix (fx, prec, name) -> Syntax.Mixfix (create_operator fx prec name)
  | Presyntax.Quit -> Syntax.Quit

let file env = List.map (fun x -> toplevel_cmd env x)
