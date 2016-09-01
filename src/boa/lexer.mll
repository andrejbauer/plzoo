{
  open Parser
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    "#" [^'\n']* '\n'   { incr_linenum lexbuf; token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
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
