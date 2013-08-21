(** Top level *)

exception BadArgs;;

(** Possible command-line options.  Ocaml automatically adds
    -help and --help.
*)
let command_line_options = []

(** One-line usage message *)
let usage_msg = 
  "Usage:  " ^ Sys.argv.(0) ^ " [filenames] \n\n" ^
    "Shell commands: \n" ^
    "\\q quit (Ctrl-D also works)\n" ^
    "\\h display this help\n" ^
    "\\d show current definitions\n" ^
    "\\e toggle eager/lazy evaluation\n" ^
    "\\l toggle evaluation inside abstractions\n" ^
    "Syntax:\n" ^
    "expr ::= ident | expr expr | ^ ident1 ... identN . expr"

(** A list of files to process, stored in REVERSE order *)
let filenames = ref [] 

(** Add a file specified on the command-line to the list
    of files to process *)
let addFile strng = 
  filenames := strng :: !filenames

(* Helper function:  evaluate a given filename *)
let read fn =
  let fin = open_in fn in
  let lexbuf = Lexing.from_channel fin in
  try
    let e = Parser.script Lexer.token lexbuf in
      (close_in fin ; e)
  with
    Message.Parse(_,_) ->
      let pos = lexbuf.Lexing.lex_curr_p in
      begin
        print_string "Syntax error detected at line ";
        print_string ( string_of_int pos.Lexing.pos_lnum );
        print_string " column ";
        print_string ( string_of_int ( pos.Lexing.pos_cnum - 
					 pos.Lexing.pos_bol ) );
        print_string "\n";
        raise Parsing.Parse_error 
      end

(* Helper function:  Parses a line of input. *)
let parse str = Parser.shell Lexer.token (Lexing.from_string str);;


(** Main function for evaluating files. Takes a filename and the
  current "rules". Successively processes each file in turn, using
  updated rules to process the following file. Thus, dependencies
  between files are allowed, as long filenames are given in an order
  that respects dependencies.
*)
let process fns = List.fold_left (fun env fn -> Eval.eval_list env (read fn)) [] fns
	
(** MAIN PROGRAM *)

let main =
  try
    (** Parse all the command-line options and store the names
      of all the files to be processed *)
    Arg.parse_argv Sys.argv command_line_options addFile usage_msg ;
    
    (** Then process the files *)
    let env = ref (process (List.rev !filenames)) in
      (** And enter the shell *)
      print_endline "lambda" ;
      print_endline "Type \\h for help." ;
      Sys.catch_break true ;
      while true do
	try begin
	  print_string "> " ;
	  let s = read_line () in
	    match s with
		"\\h" -> print_endline usage_msg
	      | "\\q" ->
		  raise End_of_file
	      | "\\d" ->
		  List.iter (fun (x,e) -> print_endline (x ^ " = " ^ (Syntax.string_of_expr e))) !env
	      | "\\e" ->
		  Eval.eager := not !Eval.eager ;
		  print_endline ("Eager evaluation is now " ^
				   (if !Eval.eager then "on" else "off") ^ ".")
	      | "\\l" ->
		  Eval.hnf := not !Eval.hnf ;
		  print_endline ("Evaluation inside abstraction is now " ^
				   (if !Eval.hnf then "on" else "off") ^ ".")
	      |  _ -> match parse s with
		      None -> ()
		    | Some c -> env := Eval.eval_command !env c
	end with
	    End_of_file -> exit 0
	  | Eval.Interrupted e ->
	      print_endline "Interrupted."
	  | e -> print_endline ("Exception: " ^ (Printexc.to_string e))
      done
  with
      Arg.Bad s
    | Arg.Help s -> prerr_endline s
