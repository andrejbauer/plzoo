(** Toplevel interactive loop. *)

(** The toplevel accepts global value definitions and expressions,
    separated by double semicolon [;;] when contained in a file.

    A global value definition [let x = e] defines a value [x].
*)

(** Usage:

    [miniml] runs the interactive loop.

    [miniml dat1 ... datN] evaluates the contents of files
    [dat1],...,[datN] then runs the interactive loop.

    [miniml -n dat1 ..., datN] evaluates the contents of files
    [dat1],...,[datN] and exits.
*)

open Syntax

(** A context describing the types of globally defined values. *)
type context = (name * ty) list

(** An environment describing globally defined values. *)
type env = (name * Machine.mvalue) list

    
(** [exec_cmd (ctx, env) cmd] executes the toplevel command [cmd] and
    returns the new context-environment pair and a string representing the
    result of evaluation. *)
let exec_cmd (ctx, env) = function
    Expr e ->
      (* check the type of [e], compile it, and run it. *)
      let ty = Type_check.type_of ctx e in
      let frm = Compile.compile e in
      let v = Machine.run frm env in
	((ctx, env),
	 "- : " ^ (Syntax.string_of_type ty) ^ " = " ^ (Machine.string_of_mvalue v))
  | Def (x, e) ->
      (* check the type of [e], compile it, run it, and return a new
	 context-environemtn pair with [x] defined as [e]. *)
      let ty = Type_check.type_of ctx e in
      let frm = Compile.compile e in
      let v = Machine.run frm env in
	(((x,ty)::ctx, (x,v)::env),
	 x ^ " : " ^ (Syntax.string_of_type ty) ^ " = " ^
	   (Machine.string_of_mvalue v))
;;

(** [exec_cmds (ctx, env) cmds] executes a list of commands in the inital
    context [ctx] and environment [env] and returns the new context and
    environment. *)
let exec_cmds ce cmds =
  List.fold_left
    (fun ce cmd -> let (ce', msg) = exec_cmd ce cmd in print_endline msg ; ce')
    ce cmds
;;

(** [shell ctx env] is the interactive shell. Here [ctx] and [env] are
    the context and environment of global definitions. *)
let shell ctx env =
  print_string ("MiniML. Press ") ;
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
	    print_string "MiniML> ";
	    let str = read_line () in
	    let cmds = Parser.toplevel Lexer.token (Lexing.from_string str) in
	    let (ctx, env) = exec_cmds (!global_ctx, !global_env) cmds in
	      (* set the new values of the global context and environment *)
	      global_ctx := ctx ;
	      global_env := env
	  with
	    | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
	    | Machine.Machine_error msg -> print_endline ("Runtime error: " ^ msg)
	    | Failure _ | Parsing.Parse_error -> print_endline "Syntax error."
      done 
    with
	End_of_file -> print_endline "\nGood bye."

(** The main program. *)
let main =
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell")]
      (fun f -> files := f :: !files)
      "Usage: miniml [-n] [file] ..." ;
    try
      let ctx, env =
	List.fold_left
	  (fun ce f ->
	     let fh = open_in f in
	     let cmds = Parser.toplevel Lexer.token (Lexing.from_channel fh) in
	       close_in fh ;
	       exec_cmds ce cmds)
	  ([],[]) !files
      in    
	if not !noninteractive then shell ctx env
    with
      | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
      | Machine.Machine_error msg -> print_endline ("Runtime error: " ^ msg)
      | Failure _ | Parsing.Parse_error -> print_endline "Syntax error."

