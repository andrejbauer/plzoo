{
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let variable = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    "#" [^'\n']* '\n'   { incr_linenum lexbuf; token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | '-'? ['0'-'9']+ { Parser.NUMERAL (int_of_string(lexeme lexbuf)) }
  | "true"          { Parser.TRUE }
  | "false"         { Parser.FALSE }
  | "skip"          { Parser.SKIP }
  | "if"            { Parser.IF }
  | "then"          { Parser.THEN }
  | "else"          { Parser.ELSE }
  | "end"           { Parser.END }
  | "while"         { Parser.WHILE }
  | "do"            { Parser.DO }
  | "done"          { Parser.DONE }
  | "print"         { Parser.PRINT }
  | "new"           { Parser.NEW }
  | "in"            { Parser.IN }
  | "and"           { Parser.AND }
  | "or"            { Parser.OR }
  | "not"           { Parser.NOT }
  | ":="            { Parser.ASSIGN }
  | ';'             { Parser.SEMICOLON }
  | '('             { Parser.LPAREN }
  | ')'             { Parser.RPAREN }
  | '+'             { Parser.PLUS }
  | '-'             { Parser.MINUS }
  | '*'             { Parser.TIMES }
  | '/'             { Parser.DIVIDE }
  | '%'             { Parser.REMAINDER }
  | '='             { Parser.EQUAL }
  | '<'             { Parser.LESS }
  | variable        { Parser.VARIABLE (lexeme lexbuf) }
  | eof             { Parser.EOF }

{
}
