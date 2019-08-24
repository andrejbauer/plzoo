{
  open Parser
  open Lexing
}

let const = ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let var = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*


rule token = parse
    '#' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | "?-"            { GOAL }
  | ":-"            { FROM }
  | "true"          { TRUE }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | const           { CONST (lexeme lexbuf) }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
