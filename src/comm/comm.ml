(** The main program. *)

module CalcVar = Zoo.Main(struct
  let name = "comm"

  type command = Syntax.command

  (** There is no top-level environment as all variables are local *)
  type environment = unit

  (** Should we show compiled code? *)
  let show_code = ref false

  (** RAM size *)
  let ram_size = ref 64

  let options = [
      ("--ram",
       Arg.Set_int ram_size,
       Format.sprintf "<size> RAM size (default: %d)" !ram_size);
      ("--code",
       Arg.Set show_code,
       " Print compiled code")
  ]

  (** At the beginning no variables are defined. *)
  let initial_environment = ()

  let read_more _ = false

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.program Lexer.token)

  (** The command that actually executes a command. It accepts an argument which we can
      ignore, a flag indicating whether we are in ineractive mode, an environment, and a
      command to be excuted. *)
  let exec _ cmd =
    let code = Compile.compile cmd in
    if !show_code then Format.printf "%t@." (Machine.print_code code) ;
    let state = Machine.make code !ram_size in
    try
      Machine.run state
    with
      Machine.Error err -> Zoo.error ~kind:"runtime error" "%t" (Machine.print_error err)
end) ;;

CalcVar.main ()
