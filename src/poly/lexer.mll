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
    '#' [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | '-'? ['0'-'9']+ { INT (int_of_string(lexeme lexbuf)) }
  | "else"          { ELSE }
  | "false"         { FALSE }
  | "fst"           { FST }
  | "fun"           { FUN }
  | "if"            { IF }
  | "is"            { IS }
  | "let"           { LET }  
  | "match"         { MATCH }
  | "rec"           { REC }
  | "snd"           { SND }
  | "then"          { THEN }
  | "true"          { TRUE }
  | "with"          { WITH }
  | "=>"            { DARROW }
  | "::"            { CONS }
  | ";;"            { SEMICOLON2 }
  | '%'             { MOD }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | ','             { COMMA }
  | '-'             { MINUS }
  | '/'             { DIVIDE }
  | '<'             { LESS }
  | '='             { EQUAL }
  | '['             { LBRACK }
  | ']'             { RBRACK }
  | '|'             { ALTERNATIVE }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
