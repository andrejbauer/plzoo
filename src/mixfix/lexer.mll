{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | '-'?['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "bool"          { TBOOL }
  | "false"         { FALSE }
  | "fst"           { FST }
  | "fun"           { FUN }
  | "$cond"         { COND }
  | "int"           { TINT }
  | "is"            { IS }
  | "let"           { LET }  
  | "list"          { TLIST }
  | "match"         { MATCH }
  | "rec"           { REC }
  | "snd"           { SND }
  | "true"          { TRUE }
  | ":quit"         { QUIT }
  | "with"          { WITH }
  | "->"            { TARROW }
  | "=>"            { DARROW }
  | "$cons"         { CONS }
  | "$pair"         { PAIR }
  | ";;"            { SEMICOLON2 }
  | "$mod"          { MOD }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "$times"        { TIMES }
  | "$plus"         { PLUS }
  | "$minus"        { MINUS } 
  | "$divide"       { DIVIDE }
  | ':'             { COLON }
  | '*'             { STAR }
  | "$less"         { LESS }
  | '='             { SET_EQUAL }
  | "$equal"        { EQUALS }
  | '['             { LBRACK }
  | ']'             { RBRACK }
  | '|'             { ALTERNATIVE }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
