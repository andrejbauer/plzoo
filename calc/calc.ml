(** The main program. *)

(** The end of file character. *)
let eof =
  match Sys.os_type with
      "Unix" | "Cygwin" -> "Ctrl-D"
    | "Win32" -> "Ctrl-Z"
    | _ -> "\"end of file\""
;;

(** The startup message. *)
let startup = "Calculator. Press " ^ eof ^ " to quit."
;;

(** Top level reads input, parses, evaluates and prints the result. *)
let main =
  print_endline startup ;
  try
    while true do
      print_string "> ";
      let str = read_line () in
        try
  	  let e = Parser.toplevel Lexer.lexeme (Lexing.from_string str) in
	  let n = Eval.eval e in
	  print_endline (string_of_int n)
        with
	  Failure str -> print_endline ("Error: " ^ str)
	| Parsing.Parse_error -> print_endline "Syntax error."
    done 
  with
    End_of_file -> print_endline "\nGood bye."
;;
