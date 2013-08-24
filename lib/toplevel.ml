(** Toplevel. *)

module type LANGUAGE =
sig
  type toplevel    (* Toplevel entries. *)
  type environment (* Runtime environment. *)

  val initial_environment : environment (* The initial environment. *)

  val prompt : string (* The prompt to show at toplevel. *)
  val more_prompt : string (* The prompt to show when reading some more. *)

  val lexer : unit (* The lexer. *)

  val file_parser : unit -> Lexing.lexbuf -> 'a (* The file parser *)
  val toplevel_parser : unit -> Lexing.lexbuf -> 'a (* Interactive shell parser *)

  val name : string (* The name of the language *)
  val version : string (* The language version *)
  val greeting : string (* The greeting printed by the interactive toplevel. *)
  val options : (Arg.key * Arg.spec * Arg.doc) list (* Language-specific command-line options *)
  val help_directive : string option (* What to type in toplevel to get help. *)

  val exec : bool -> environment -> toplevel -> environment (* Execute a toplevel directive. *)
  val read_more : string -> bool (* Given the input so far, should we read more in the interactive shell? *)
end

module Make(L : LANGUAGE) =
struct

  (** Should the interactive shell be run? *)
  let interactive_shell = ref true

  (** The command-line wrappers that we look for. *)
  let wrapper = ref (Some ["rlwrap"; "ledit"])

  (** The usage message. *)
  let usage = "Usage: " ^ L.name ^ " [option] ... [file] ..."

  (** A list of files to be loaded and run. *)
  let files = ref []

  (** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
  let add_file interactive filename = (files := (filename, interactive) :: !files)

  (** Command-line options *)
  let options = Arg.align [
    ("--wrapper",
     Arg.String (fun str -> wrapper := Some [str]),
     "<program> Specify a command-line wrapper to be used (such as rlwrap or ledit)");
    ("--no-wrapper",
     Arg.Unit (fun () -> wrapper := None),
     " Do not use a command-line wrapper");
    ("-v",
     Arg.Unit (fun () ->
       print_endline (L.name ^ " " ^ L.version ^ "(" ^ Sys.os_type ^ ")");
       exit 0),
     " Print version information and exit");
    ("-n",
     Arg.Clear interactive_shell,
     " Do not run the interactive toplevel");
    ("-l",
     Arg.String (fun str -> add_file false str),
     "<file> Load <file> into the initial environment")
  ] @
  L.options

  (** Treat anonymous arguments as files to be run. *)
  let anonymous str =
    add_file true str;
    interactive_shell := false

  (** Parse the contents from a file, using a given [parser]. *)
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
      Error.Error err -> close_in fh ; raise (Error.Error err)
  with
    (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> Error.fatal ~loc:Position.Nowhere "%s" msg

  (** Parse input from toplevel, using the given [parser]. *)
  let read_toplevel parser () =
    let rec read_more prompt acc =
      if L.read_more acc
      then begin
        print_string prompt ;
        let str = read_line () in
          read_more L.more_prompt (acc ^ "\n" ^ str)
      end
      else acc
    in
    let str = read_more L.prompt "" in
    let lex = Lexing.from_string (str ^ "\n") in
      parser lex

  (** Parser wrapper that catches syntax-related errors and converts them to errors. *)
  let wrap_syntax_errors parser lex =
    try
      parser L.lexer lex
    with
      | Failure "lexing: empty token" ->
        Error.syntax ~loc:(Position.position_of_lex lex) "unrecognised symbol"
      | _ ->
        Error.syntax ~loc:(Position.position_of_lex lex) "general confusion"

  (** Load directives from the given file. *)
  let use_file ctx (filename, interactive) =
    let cmds = read_file (wrap_syntax_errors L.file_parser) filename in
      List.fold_left (L.exec interactive) ctx cmds

  (** Interactive toplevel *)
  let toplevel ctx =
    let eof = match Sys.os_type with
      | "Unix" | "Cygwin" -> "Ctrl-D"
      | "Win32" -> "Ctrl-Z"
      | _ -> "EOF"
    in
      print_endline L.greeting ;
      match L.help_directive with
        | Some h -> print_endline ("[Type " ^ eof ^ " to exit or \"" ^ h ^ "\" for help.]") ;
        | None -> print_endline ("[Type " ^ eof ^ " to exit.]") ;
      try
        let ctx = ref ctx in
          while true do
            try
              let cmd = read_toplevel (wrap_syntax_errors L.toplevel_parser) () in
                ctx := L.exec true !ctx cmd
            with
              | Error.Error err -> Error.print err
              | Sys.Break -> prerr_endline "Interrupted."
          done
      with End_of_file -> ()

  (** Main program *)
  let main () =
    Sys.catch_break true;
    (* Parse the arguments. *)
    Arg.parse options anonymous usage;
    (* Attempt to wrap yourself with a line-editing wrapper. *)
    if !interactive_shell then
      begin match !wrapper with
        | None -> ()
        | Some lst ->
          let n = Array.length Sys.argv + 2 in
          let args = Array.make n "" in
            Array.blit Sys.argv 0 args 1 (n - 2);
            args.(n - 1) <- "--no-wrapper";
            List.iter
              (fun wrapper ->
                try
                  args.(0) <- wrapper;
                  Unix.execvp wrapper args
                with Unix.Unix_error _ -> ())
              lst
      end;
    (* Files were listed in the wrong order, so we reverse them *)
    files := List.rev !files;
    (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
    Format.set_max_boxes 42 ;
    Format.set_ellipsis_text "..." ;
    try
      (* Run and load all the specified files. *)
      let ctx = List.fold_left use_file L.initial_environment !files in
        if !interactive_shell then toplevel ctx
    with
        Error.Error err -> Error.print err; exit 1

end
