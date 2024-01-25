

type associativity = LeftAssoc | RightAssoc | NonAssoc
and fixity = Prefix | Postfix | Infix of associativity

type operator = {
  tokens: string list;
  fx : fixity;
  prec: int;
  expression: Syntax.expr
}

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

let mixfix_define (command_used:string)
  (operator_tokens_string:string) (precedence:int) (args: (string*Syntax.htype) list)
  (body:Syntax.expr): unit =
  let operator_tokens = List.filter (fun x -> x <> "") (String.split_on_char '_' operator_tokens_string) in
  let operator = {tokens = operator_tokens; prec = precedence; fx = determine_fixity command_used operator_tokens_string; expression = Syntax.Lambda(args, body)} in
  let new_operators = operator::!Syntax.operators in
  Syntax.operators := new_operators