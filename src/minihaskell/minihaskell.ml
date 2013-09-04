(** MiniHaskell toplevel. *)

open Message
open Syntax

(**
  The toplevel accepts global value definitions [let x = e] and expressions,
  separated by double semicolons [;;] when contained in a file.

  Usage:

    [minihaskell] runs the interactive loop

    [minihaskell dat1 ... datN] evaluates the contents of files
    [dat1], ..., [datN] then runs the interactive loop.

    [minihaskell -n dat1 ..., datN] evaluates the contents of files
    [dat1],...,[datN] and exits.
*)

exception Fatal_error of string

let fatal_error msg = raise (Fatal_error msg)

(** [exec_cmd (ctx, env) n cmd] executes the toplevel command [cmd] in
    the given context [ctx] and environment [env]. It forces
    evaluation of up to [n] levels of nesting of pairs and lists. It
    returns the new context and environment. *)
let rec exec_cmd n (ctx, env) = function
    Expr e ->
      (* type check [e], evaluate, and print result *)
      let ty = Type_check.type_of ctx e in
      let v = Interpret.interp env e in
	print_string ("- : " ^ string_of_type ty ^ " = ") ;
	Interpret.print_result n v ;
	print_newline () ;
	(ctx, env)
  | Def (x, e) ->
      (* type check [e], and store it unevaluated! *)
      let ty = Type_check.type_of ctx e in
	print_endline ("val " ^ x ^ " : " ^ string_of_type ty) ;
	((x,ty)::ctx, (x, ref (Interpret.VClosure (env,e)))::env)
  | Quit -> raise End_of_file
  | Use fn -> exec_file n (ctx, env) fn


(** [exec_file (ctx, env) n fn] executes the contents of file [fn] in
    the given context [ctx] and environment [env]. It forces
    evaluation of up to [n] levels of nesting of pairs and lists. It
    returns the new context and environment. *)
and exec_file n ce fn =
  let fh = open_in fn in
  let lex = Message.lexer_from_channel fn fh in
    try
      let cmds = Parser.toplevel Lexer.token lex in
	close_in fh ;
	exec_cmds n ce cmds
    with
	Type_check.Type_error msg -> fatal_error (fn ^ ":\n" ^ msg)
      | Interpret.Runtime_error msg -> fatal_error msg
      | Sys.Break -> fatal_error "Interrupted."
      | Parsing.Parse_error | Failure("lexing: empty token") ->
	  fatal_error (Message.syntax_error lex)

(** [exec_cmds (ctx, env) n cmds] executes the list of toplevel
    commands [cmd] in the given context [ctx] and environment
    [env]. It forces evaluation of up to [n] levels of nesting of
    pairs and lists. It returns the new context and environment. *)
and exec_cmds n ce cmds =
  List.fold_left (exec_cmd n) ce cmds
;;

(** [shell ctx env] is the interactive shell. Here [ctx] and [env] are
    the context and environment of global definitions. *)
let shell n ctx env =
  print_string ("MiniHaskell. Press ") ;
  print_string (match Sys.os_type with
		    "Unix" | "Cygwin" -> "Ctrl-D"
		  | "Win32" -> "Ctrl-Z"
		  | _ -> "EOF") ;
  print_endline " to exit." ;
  let global_ctx = ref ctx in
  let global_env = ref env in
    try
      while true do
	try
	  (* read a line, parse it and exectute it *)
	  print_string "MiniHaskell> ";
	  let str = read_line () in
	  let lex = Message.lexer_from_string str in
	  let cmds =
	    try
	      Parser.toplevel Lexer.token lex
	    with
	      | Failure("lexing: empty token")
	      | Parsing.Parse_error -> fatal_error (Message.syntax_error lex)
	  in
	  let (ctx, env) = exec_cmds n (!global_ctx, !global_env) cmds in
	    (* set the new values of the global context and environment *)
	    global_ctx := ctx ;
	    global_env := env
	with
	    Fatal_error msg -> Message.report msg
	  | Interpret.Runtime_error msg -> Message.report msg
	  | Type_check.Type_error msg -> Message.report msg
	  | Sys.Break -> Message.report ("Interrupted.")
      done 
    with
	End_of_file -> print_endline "\nGood bye."

(** The main program. *)
let main =
  Sys.catch_break true ;
  let print_depth = ref 100 in
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell");
       ("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]
      (fun f -> files := f :: !files)
      "Usage: minihaskell [-p <int>] [-n] [file] ..." ;
    files := List.rev !files ;
    let ctx, env =
      try
	List.fold_left (exec_file !print_depth) ([],[]) !files
      with
	  Fatal_error msg -> Message.report msg ; exit 1
    in    
      if not !noninteractive then shell !print_depth ctx env
