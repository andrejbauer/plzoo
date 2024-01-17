(* Syntax  *)


type associativity = LeftAssoc | RightAssoc | NonAssoc
and fixity = Prefix | Postfix | Infix of associativity

type operator = {
  tokens: string list;
  fx : fixity;
  prec: int;
  expression: Syntax.expr
}

let rec expr = function
  | Presyntax.Var x -> Syntax.Var x
  | Presyntax.Int k -> Syntax.Int k
  | Presyntax.Bool b -> Syntax.Bool b
  | Presyntax.Times (e1, e2) -> Syntax.Times (expr e1, expr e2)
  | Presyntax.Divide (e1, e2) -> Syntax.Divide (expr e1, expr e2)
  | Presyntax.Mod (e1, e2) -> Syntax.Mod (expr e1, expr e2)
  | Presyntax.Plus (e1, e2) -> Syntax.Plus (expr e1, expr e2)
  | Presyntax.Minus (e1, e2) -> Syntax.Minus (expr e1, expr e2)
  | Presyntax.Equal (e1, e2) -> Syntax.Equal (expr e1, expr e2)
  | Presyntax.Less (e1, e2) -> Syntax.Less (expr e1, expr e2)
  | Presyntax.If (e1, e2, e3) -> Syntax.If (expr e1, expr e2, expr e3)
  | Presyntax.Fun (x, ht, e) -> Syntax.Fun (x, ht, expr e)
  | Presyntax.Seq es -> seq es
  | Presyntax.Pair (e1, e2) -> Syntax.Pair (expr e1, expr e2)
  | Presyntax.Fst e -> Syntax.Fst (expr e)
  | Presyntax.Snd e -> Syntax.Snd (expr e)
  | Presyntax.Rec (f, x, e1) -> Syntax.Rec (f, x, expr e1)
  | Presyntax.Nil ht -> Syntax.Nil ht
  | Presyntax.Cons (e1, e2) -> Syntax.Cons (expr e1, expr e2)
  | Presyntax.Match (e, ht, e1, x, y, e2) -> Syntax.Match (expr e, ht, expr e1, x, y, expr e2)
      (** list decomposition [match e with [t] -> e1 | x::y -> e2] *)

and seq (es: Presyntax.expr list): Syntax.expr =
  (* Fold left wants initial value, which we dont really have? *)
  match es with
    | [] -> failwith "seq should be non-empty" 
    | a::[] -> expr a
    | head ::tail -> Syntax.Apply (expr head, seq tail)

let toplevel_cmd (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
  match cmd with
  | Presyntax.Expr e -> Syntax.Expr (expr e)
  | Presyntax.Def (name,  e) -> Syntax.Def (name, expr e)
  | Presyntax.MixDef (mixassoc,mixname, prec, e) -> Syntax.MixDef (mixassoc, mixname, prec, (expr e) )
  | Presyntax.Quit -> Syntax.Quit


let determine_assoc = function
  | "mixn" -> NonAssoc
  | "mixl" -> LeftAssoc
  | "mixr" -> RightAssoc
  | _ -> failwith "Invalid or missing associativity for infix operator"
  
let determine_fixity (command:string) (s:string): fixity = 
  let first_char = String.get s 0 in
  match first_char with
  | '_' -> Prefix
  | _ -> let last_char = String.get s (String.length s - 1) in
    match last_char with
    | '_' -> Postfix
    | _ -> Infix (determine_assoc command)
;;

let create_operator md: operator =
  let (command, mixname, prec, e) = md in
    let fixity = determine_fixity command mixname in 
    let name = List.filter (fun x -> x <> "") (String.split_on_char '_' mixname) in
    {tokens = name; prec = prec; fx = fixity; expression = e}

let file = List.map toplevel_cmd
;;