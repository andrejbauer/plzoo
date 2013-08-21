{
  open Parser
}

rule lexeme = parse
    [' ' '\t' '\r' '\n']  { lexeme lexbuf }
  | ['0'-'9']+  { NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIVIDE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof         { EOF }
