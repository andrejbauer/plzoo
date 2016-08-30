{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+           { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"                { TINT }
  | "bool"               { TBOOL }
  | "true"               { TRUE }
  | "false"               { FALSE }
  | "fun"           { FUN }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }  
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
