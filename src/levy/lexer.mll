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
  | "(*"            { comment 0 lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | '-'? ['0'-'9']+ { INT (int_of_string(lexeme lexbuf)) }

  | 'U'             { TFORGET }
  | 'F'             { TFREE }
  | "->"            { TARROW }
  | "bool"          { TBOOL }
  | "int"           { TINT }

  | "do"            { DO }
  | "else"          { ELSE }
  | "false"         { FALSE }
  | "force"         { FORCE }
  | "fun"           { FUN }
  | "if"            { IF }
  | "in"            { IN }
  | "is"            { IS }
  | "let"           { LET }  
  | "rec"           { REC }
  | "return"        { RETURN }
  | "then"          { THEN }
  | "thunk"         { THUNK }
  | "true"          { TRUE }

  | ";;"            { SEMISEMI }

  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | ':'             { COLON }
  | '<'             { LESS }
  | '='             { EQUAL }
  | "=>"            { DARROW }
  | "<-"            { ASSIGN }

  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

and comment n = parse
  | "*)"                { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
  | "(*"                { comment (n + 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment n lexbuf }
  | _                   { comment n lexbuf }
  | eof                 { Zoo.error ~loc:(Zoo.location_of_lex lexbuf) "Unterminated comment" }


{
}
