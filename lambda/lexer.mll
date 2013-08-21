(** The lexical structure of ulc *)

{
  open Parser

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let ident = ['a'-'z' 'A'-'Z' '0'-'9' '_']+ '\''*

rule token = parse
  | '#' [^ '\n']* '\n' { incr_linenum lexbuf; incr Message.lineno; token lexbuf }
  | '\n'               { incr_linenum lexbuf; incr Message.lineno; token lexbuf }
  | [' ' '\t' '\r']    { token lexbuf }
  | ident              { IDENT (Lexing.lexeme lexbuf) }
  | '='                { EQUAL }
  | '^'                { LAMBDA }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '.'                { PERIOD }
  | eof                { EOF }

(* trailer *)
{
}

