(** Miniprolog toplevel. *)

open Message
open Syntax

(**
  The toplevel accepts global value definitions [let x = e] and expressions,
  separated by double semicolons [;;] when contained in a file.

  Usage:

    [miniprolog] runs the interactive loop

    [miniprolog dat1 ... datN] evaluates the contents of files
    [dat1], ..., [datN] then runs the interactive loop.

    [miniprolog -n dat1 ..., datN] evaluates the contents of files
    [dat1],...,[datN] and exits.
*)

exception Fatal_error of string

let fatal_error msg = raise (Fatal_error msg)

(** [exec_cmd cmd] executes the toplevel command [cmd]. *)
let rec exec_cmd = function
  | Goal g -> Solve.solve_toplevel g
  | Assert a ->  Solve.assertz a
  | Quit -> raise End_of_file
  | Use fn -> exec_file fn

(** [exec_file fn] executes the contents of file [fn]. *)
and exec_file fn =
  let fh = open_in fn in
  let lex = Message.lexer_from_channel fn fh in
    try
      let cmds = Parser.toplevel Lexer.token lex in
	close_in fh ;
	exec_cmds cmds
    with
      | Sys.Break -> fatal_error "Interrupted."
      | Parsing.Parse_error | Failure("lexing: empty token") ->
	  fatal_error (Message.syntax_error lex)

(** [exec_cmds cmds] executes the list of toplevel commands [cmds]. *)
and exec_cmds cmds = List.iter exec_cmd cmds
;;

(** [shell ()] is the interactive shell. *)
let shell () =
  print_string ("Miniprolog. Press ") ;
  print_string (match Sys.os_type with
		    "Unix" | "Cygwin" -> "Ctrl-D"
		  | "Win32" -> "Ctrl-Z"
		  | _ -> "EOF") ;
  print_endline " to exit." ;
  print_endline "Input syntax: " ;
  print_endline "   ?- query.           Make a query." ;
  print_endline "   a(t1, ..., tn).     Assert an atomic proposition." ;
  print_endline "   A :- B1, ..., Bn.   Assert an inference rule." ;
  print_endline "   $quit               Exit interpreter." ;
  print_endline "   $use \"filename\"     Execute commands from a file." ;
  try
    while true do
      try
	print_string "Prolog> ";
	let str = read_line () in
	let lex = Message.lexer_from_string str in
	let cmds =
	  try
	    Parser.toplevel Lexer.token lex
	  with
	    | Failure("lexing: empty token")
	    | Parsing.Parse_error -> fatal_error (Message.syntax_error lex)
	in
	  exec_cmds cmds
      with
	| Fatal_error msg -> Message.report msg
	| Sys.Break -> Message.report ("Interrupted.")
    done 
  with End_of_file -> print_endline "\nGood bye."

(** The main program. *)
let main =
  Sys.catch_break true ;
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell")]
      (fun f -> files := f :: !files)
      "Usage: miniprolog [-n] [file] ..." ;
    files := List.rev !files ;
    try
      List.iter exec_file !files ;
      if not !noninteractive then shell ()
    with Fatal_error msg -> Message.report msg
