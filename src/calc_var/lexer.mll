{
  open Parser
}

rule lexeme = parse
    [' ' '\t' '\r' '\n']  { lexeme lexbuf }
  | ['0'-'9']+  { NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']* { VARIABLE (Lexing.lexeme lexbuf) }
  | '='         { EQUAL }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIVIDE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof         { EOF }
