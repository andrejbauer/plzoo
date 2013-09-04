(** Boa toplevel. *)

open Message
open Syntax

(** Exception [Fatal_error] is raised when further evaluation is
    impossible. *)
exception Fatal_error of string

(** [fatal_error msg] raises exception [Fatal_error msg]. *)
let fatal_error msg = raise (Fatal_error msg)

(** [exec_cmd env c] executes toplevel command [c] in global
    environment [env]. It prints the result on standard output and return
    the new environment. *)
let rec exec_cmd env = function
    Expr e ->
      (* evaluate [e] *)
      let v = Eval.eval None env e in
	print_string (Eval.string_of_obj v) ;
	print_newline () ;
	env
  | Def (x, e) ->
      (* define a new global value *)
      let v = Eval.eval None env e in
	print_string (x ^ " = " ^ Eval.string_of_obj v) ;
	print_newline () ;
	(x,v)::env
  | Quit -> raise End_of_file
  | Use fn -> exec_file env fn

(** [exec file env fn] executes the contents of file [fn] in global
    environment [env]. It prints results on the standard output and
    returns the new environment. *)
and exec_file env fn =
  let fh = open_in fn in
  let lex = Message.lexer_from_channel fn fh in
    try
      let cmds = Parser.toplevel Lexer.token lex in
	close_in fh ;
	exec_cmds env cmds
    with
      | Eval.Runtime_error msg -> fatal_error ("runtime error:" ^ msg)
      | Sys.Break -> fatal_error "Interrupted."
      | Parsing.Parse_error | Failure("lexing: empty token") ->
	  fatal_error (Message.syntax_error lex)

(** [exec_cmds env cmds] executes the list of commands [cmds] in
    environment [env] and returns the new environment. *)
and exec_cmds env cmds =
  List.fold_left exec_cmd env cmds
;;

(** [shell env] runs the interactive shell in environment [env]. *)
let shell env =
  print_string ("Boa. Press ") ;
  print_string (match Sys.os_type with
		    "Unix" | "Cygwin" -> "Ctrl-D"
		  | "Win32" -> "Ctrl-Z"
		  | _ -> "EOF") ;
  print_endline " to exit." ;
  let global_env = ref env in
    try
      while true do
	try
	  print_string "Boa> ";
	  let str = read_line () in
	  let lex = Message.lexer_from_string str in
	    (* parse a list of commands and execute them *)
	  let cmds =
	    try
	      Parser.toplevel Lexer.token lex
	    with
	      | Failure("lexing: empty token")
	      | Parsing.Parse_error -> fatal_error (Message.syntax_error lex)
	  in
	  let env = exec_cmds !global_env cmds in
	    (* set the new global environment *)
	    global_env := env
	with
	  | Fatal_error msg -> Message.report msg
	  | Eval.Runtime_error msg -> Message.report ("runtime error: " ^ msg)
	  | Sys.Break -> Message.report ("Interrupted.")
      done 
    with
	End_of_file -> print_endline "\nGood bye."

(** Main program *)
let main =
  Sys.catch_break true ;
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run interactive shell")]
      (fun f -> files := f :: !files)
      "Usage: boa [-n] [file] ..." ;
    files := List.rev !files ;
    let env =
      try
	List.fold_left exec_file [] !files
      with
	  Fatal_error msg -> Message.report msg ; exit 1
    in    
      if not !noninteractive then shell env
