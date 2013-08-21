(** Toplevel. *)

open Syntax

(** A context describing the types of globally defined values. *)
type context = (name * ty) list

(** An environment describing defined boxes. *)
type env = (name * Eval.value) list

let graphviz = ref false (* Do we output graphs or values? *)

(** [exec_cmd (ctx, env) cmd] executes the toplevel command [cmd] and
    returns the new context-environment pair and a string representing the
    result of evaluation. *)
let exec_cmd (ctx, env) = function
  | Expr e ->
      (* check the type of [e] and evaluate it. *)
      let _ = Type_check.type_of ctx e in
      let output  = (if !graphviz then
                       Graph.dot_of_circuit (Graph.circuit_of_expr [] e)
                     else
                       Eval.string_of_value (Eval.eval env e))
      in
	((ctx, env), output)
  | Def (f, lst, e) ->
      (* check the type of the box, and return a new context-environment pair with [f] defined. *)
      let (ts,t) = Type_check.type_of_def ctx lst e in
      let v = Eval.compile_box env (List.map snd lst) e in
      let output = (if !graphviz then
                      Graph.dot_of_circuit ~name:f (Graph.circuit_of_expr (List.map snd lst) e)
                    else
                      f ^ " : " ^ (Syntax.string_of_func_ty ts t)
                   )
      in
      ((Type_check.extend_func ctx f ts t, Eval.extend_func env f v), output)
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
  print_string ("Circuit. Press ") ;
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
(*	    print_string "Circuit> "; *)
	    print_string "";
	    let str = read_line () in
	    let cmds = Parser.toplevel Lexer.token (Lexing.from_string str) in
	    let (ctx, env) = exec_cmds (!global_ctx, !global_env) cmds in
	      (* set the new values of the global context and environment *)
	      global_ctx := ctx ;
	      global_env := env
	  with
	    | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
            | Eval.Runtime msg -> print_endline ("Runtime error: " ^ msg)
	    | Failure _ | Parsing.Parse_error -> print_endline "Syntax error."
      done 
    with
	End_of_file -> print_endline "\nGood bye."

(** The main program. *)
let main =
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell");
       ("-g", Arg.Set graphviz, "output graphs instead of values")]
      (fun f -> files := f :: !files)
      "Usage: circuit [-n] [file] ..." ;
    try
      let ctx, env =
	List.fold_left
	  (fun ce f ->
	     let fh = open_in f in
	     let cmds = Parser.toplevel Lexer.token (Lexing.from_channel fh) in
	       close_in fh ;
	       exec_cmds ce cmds)
	  (Builtin.ctx, Builtin.env) !files
      in    
	if not !noninteractive then shell ctx env
    with
      | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
      | Eval.Runtime msg -> print_endline ("Runtime error: " ^ msg)
      | Failure _ | Parsing.Parse_error -> print_endline "Syntax error."

