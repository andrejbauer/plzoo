{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    '#' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "and"           { AND }
  | "bool"          { TBOOL }
  | "else"          { ELSE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "if"            { IF }
  | "in"            { IN }
  | "is"            { IS }
  | "int"           { TINT }
  | "let"           { LET }
  | "not"           { NOT }
  | "or"            { OR }
  | "then"          { THEN }
  | "true"          { TRUE }
  | ";;"            { SEMICOLON2 }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | '-'             { MINUS }
  | "->"            { TARROW }
  | '/'             { DIVIDE }
  | ':'             { COLON }
  | '<'             { LESS }
  | '='             { EQUAL }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
