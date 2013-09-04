{
  (** The lexer. *)

  open Parser
}

let name = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '!' '@' '$' '%' '&' '*' '-' '+' '|' '\\'
            '[' ']' '{' '}' ',' '<' '=' '>' '?' '/' '~' '`' ]+

rule token = parse
  | "/*"                { comment 0 lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']     { token lexbuf }
  | name                { let s = Lexing.lexeme lexbuf in NAME s }
  | "#context"          { CONTEXT }
  | "#help"             { HELP }
  | "#quit"             { QUIT }
  | "#constant"         { CONSTANT }
  | "#eager"            { EAGER }
  | "#lazy"             { LAZY }
  | "#deep"             { DEEP }
  | "#shallow"          { SHALLOW }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ":="                { COLONEQ }
  | '.'                 { PERIOD }
  | '^'                 { LAMBDA }
  | ';'                 { SEMI }
  | eof                 { EOF }

and comment n = parse
  | "*/"                { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
  | "/*"                { comment (n + 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment n lexbuf }
  | _                   { comment n lexbuf }
  | eof                 { Zoo.syntax_error ~loc:(Zoo.position_of_lex lexbuf) "Unterminated comment" }

{
  let read_file parser fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = parser lex in
      close_in fh;
      terms
    with
      (* Close the file in case of any parsing errors. *)
      (Zoo.Error _) as exc -> close_in fh; raise exc
  with
    (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> Zoo.fatal_error ~loc:Zoo.Nowhere "%s" msg

  let read_toplevel parser () =
    let ends_with_semi str =
      let i = ref (String.length str - 1) in
        while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
        !i >= 0 && str.[!i] = ';'
    in

    let rec read_more prompt acc =
      if ends_with_semi acc
      then acc
      else begin
        print_string prompt ;
        let str = read_line () in
          read_more "  " (acc ^ "\n" ^ str)
      end
    in

    let str = read_more "> " "" in
    let lex = Lexing.from_string (str ^ "\n") in
      parser lex
}
