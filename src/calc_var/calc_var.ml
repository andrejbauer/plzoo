module CalcVar = Zoo.Main(struct
  let name = "calc_var"

  type command = Syntax.command

  (** The toplevel keeps an associative list which maps variables to their names. *)
  type environment = (string * int) list

  let options = []

  (** At the beginning no variables are defined. *)
  let initial_environment = []

  let read_more _ = false

  let file_parser = None

  let toplevel_parser = Some (Parser.toplevel Lexer.lexeme)

  (** The command that actually executes a command. It accepts an argument which we can
      ignore, a flag indicating whether we are in ineractive mode, an environment, and a
      command to be excuted. It must return the new environment. *)
  let exec env cmd =
    match cmd with
      | Syntax.Expression e ->
        let n = Eval.eval env e in
        Zoo.print_info "%d\n" n;
        env
      | Syntax.Definition (x, e) ->
        let n = Eval.eval env e in
        (x, n) :: env
end) ;;

CalcVar.main ()
