{
}

rule lexeme = parse
  | [' ' '\t' '\r' '\n']  { lexeme lexbuf }
  | ['0'-'9']+  { Parser.NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'         { Parser.PLUS }
  | '-'         { Parser.MINUS }
  | '*'         { Parser.TIMES }
  | '/'         { Parser.DIVIDE }
  | '('         { Parser.LPAREN }
  | ')'         { Parser.RPAREN }
  | eof         { Parser.EOF }
