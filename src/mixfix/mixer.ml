(* Transcribing stuff from Presyntax to Syntax  *)

let determine_fixity assoc s =
    let first_char = String.get s 0 and last_char = String.get s (String.length s - 1) in
    match (first_char, last_char) with
    | ('_','_') -> (match assoc with
      | Presyntax.NonAssoc -> Syntax.Infix Syntax.NonAssoc
      | Presyntax.LeftAssoc -> Syntax.Infix Syntax.LeftAssoc
      | Presyntax.RightAssoc -> Syntax.Infix Syntax.RightAssoc)
    | ('_',_) -> Syntax.Postfix
    | (_, '_') -> Syntax.Prefix
    | (_, _) -> Syntax.Closed

let create_operator assoc name =
  let fx = determine_fixity assoc name in
    let tokens = (List.filter (fun x -> x <> "") (String.split_on_char '_' name)) in
    match tokens with 
    | [] -> Zoo.error ?kind:(Some "Operator error") "Empty operator"
    | tokens -> Syntax.{tokens ; fx }

let toplevel_cmd env (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
  match cmd with

  | Presyntax.Expr e ->
      Environment.dprintln "Parsing expr";
      Environment.dprintln (Presyntax.string_of_expr e);
     let e = Parser.(check_success env @@ expr env e) in
      Environment.dprintln "Parsed expr";
      Environment.dprintln (Syntax.string_of_expr e);
     Syntax.Expr e

  | Presyntax.Def (name,  e) ->
      Environment.dprintln "Parsing expr";
      Environment.dprintln (Presyntax.string_of_expr e);
     let e = Parser.(check_success env @@ expr env e) in
      Environment.dprintln "Parsed expr";
      Environment.dprintln (Syntax.string_of_expr e);
     Syntax.Def (name, e)

  | Presyntax.Mixfix (assoc, prec, name) ->
     Syntax.Mixfix (prec, create_operator assoc name)

  | Presyntax.Quit ->
     Syntax.Quit

let file env = List.map (fun x -> toplevel_cmd env x)
