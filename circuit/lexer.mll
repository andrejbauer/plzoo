{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | '-'? ['0'-'9']+      { INT (int_of_string(Lexing.lexeme lexbuf)) }
	| "as"            { AS }
  | "bool"          { TYBOOL }
  | "box"           { BOX }
  | "copy"          { COPY }
  | "else"          { ELSE }
  | "false"         { BOOL false }
  | "if"            { IF }
  | "in"            { IN }
  | "int"           { TYINT }
  | "let"           { LET }
  | "string"        { TYSTRING }
  | "then"          { THEN }  
  | "true"          { BOOL true }
  | '='             { EQUAL }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | ';'             { SEMICOLON }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | '"' [^'"']* '"' { let s = Lexing.lexeme lexbuf in 
	                      STRING (String.sub s 1 (String.length s -2)) }
  | eof             { EOF }

{
}
