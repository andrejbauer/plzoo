(** A list of command-line wrappers to look for. *)
let wrapper = ref (Some ["rlwrap"; "ledit"])

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
      print_endline ("tt " ^ Version.version ^ "(" ^ Sys.os_type ^ ")");
      exit 0),
    " Print version information and exit");
  ("-V",
   Arg.Int (fun k -> Print.verbosity := k),
   "<int> Set verbosity level");
  ("-n",
    Arg.Clear interactive_shell,
    " Do not run the interactive toplevel");
  ("-l",
    Arg.String (fun str -> add_file false str),
    "<file> Load <file> into the initial environment");
]

(** Treat anonymous arguments as files to be run. *)
let anonymous str =
  add_file true str;
  interactive_shell := false

(** [exec_cmd ctx d] executes toplevel directive [d] in context [ctx]. It prints the
    result if in interactive mode, and returns the new context. *)
let rec exec_cmd interactive ctx (d, loc) =
  match d with
    | Syntax.expr e ->
      let e = Desugar.expr ctx.names e in
      let t = Typing.infer ctx e in
      let e = Norm.nf ctx e in
        if interactive then
          Format.printf "    = %t@\n    : %t@."
            (Print.expr ctx.names e)
            (Print.expr ctx.names t) ;
        ctx
    | Syntax.Definition (x, e) ->
      ignore
        (List.fold_left
           (fun k x ->
             (match Context.lookup k ctx with
               | Parameter t ->
                 Format.printf "@[%s : %t@]@." x (Print.expr ctx.names t)
               | Definition (t, e) ->
                 Format.printf "@[%s = %t@]@\n    : %t@." x (Print.expr ctx.names e) (Print.expr ctx.names t)) ;
             k + 1)
           0 ctx.names) ;
      ctx
    | Input.TopLet (x, c) ->
      if List.mem x ctx.names then Error.typing ~loc "%s already exists" x ;
      let c = Desugar.computation ctx.names c in
        Machine.toplet ctx x c
    | Input.TopParam (xs, t) ->
      let t = Desugar.expr ctx.names t in
        ignore (Typing.check_sort ctx t) ;
        let ctx, _ = List.fold_left
          (fun (ctx, t) x ->
            if List.mem x ctx.names then Error.typing ~loc "%s already exists" x ;
            if interactive then Format.printf "%s is assumed.@." x ;
            (add_parameter x t ctx, Syntax.shift 1 t))
          (ctx, t) xs
        in
          ctx
    | Input.TopDefine (x, e) ->
      if List.mem x ctx.names then Error.typing ~loc "%s already exists" x ;
      let e = Desugar.expr ctx.names e in
      let t = Typing.infer ctx e in
        if interactive then
          Format.printf "%s is defined.@." x ;
        add_definition x t e ctx
    | Input.Computation c ->
      let c = Desugar.computation ctx.names c in
      let v = Machine.toplevel ctx c in
        if interactive then
          Format.printf "%t@." (Print.value ctx.names v) ;
        ctx
    | Input.Help ->
      print_endline help_text ; ctx
    | Input.Quit -> exit 0

(** Load directives from the given file. *)
and use_file ctx (filename, interactive) =
  let cmds = Lexer.read_file (parse Parser.file) filename in
    List.fold_left (exec_cmd interactive) ctx cmds

(** Interactive toplevel *)
let toplevel ctx =
  let eof = match Sys.os_type with
    | "Unix" | "Cygwin" -> "Ctrl-D"
    | "Win32" -> "Ctrl-Z"
    | _ -> "EOF"
  in
  print_endline ("tt " ^ Version.version);
  print_endline ("[Type " ^ eof ^ " to exit or \"#help;;\" for help.]");
  try
    let ctx = ref ctx in
    while true do
      try
        let cmd = Lexer.read_toplevel (parse Parser.commandline) () in
        ctx := exec_cmd true !ctx cmd
      with
        | Error.Error err -> Print.error err
        | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> ()

(** Main program *)
let main =
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
    let ctx = List.fold_left use_file empty_context !files in
    if !interactive_shell then toplevel ctx
  with
    Error.Error err -> Print.error err; exit 1
