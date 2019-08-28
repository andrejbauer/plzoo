{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    "#" [^'\n']* '\n'   { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "and"           { AND }
  | "copy"          { COPY }
  | "else"          { ELSE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "if"            { IF }
  | "in"            { IN }
  | "let"           { LET }  
  | "not"           { NOT }
  | "or"            { OR }
  | "skip"          { SKIP }
  | "then"          { THEN }
  | "this"          { THIS }
  | "true"          { TRUE }
  | "with"          { WITH }
  | "->"            { ARROW }
  | ":="            { ASSIGN }
  | ";"             { SEMICOLON }
  | ";;"            { SEMICOLON2 }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '%'             { REMAINDER }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "<>"            { UNEQUAL }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
