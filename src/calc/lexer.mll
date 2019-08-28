{
}

rule lexeme = parse
  | [' ' '\t' '\r']  { lexeme lexbuf }
  | '\n'        { Lexing.new_line lexbuf; lexeme lexbuf }
  | ['0'-'9']+  { Parser.NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'         { Parser.PLUS }
  | '-'         { Parser.MINUS }
  | '*'         { Parser.TIMES }
  | '/'         { Parser.DIVIDE }
  | '('         { Parser.LPAREN }
  | ')'         { Parser.RPAREN }
  | eof         { Parser.EOF }
