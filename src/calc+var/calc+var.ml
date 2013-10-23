module Calc = Zoo.Toplevel(struct
  (** The toplevel evaluates commands, as described by the type [Syntax.command]. *)
  type toplevel = Syntax.command

  (** The toplevel keeps an associative list which maps variables to their names. *)
  type environment = (string * int) list

  let name = "calc+var"

  let options = []
  let help_directive = None

  (** At the beginning no variables are defined. *)
  let initial_environment = []

  let prompt = "> "
  let more_prompt = "> "

  let read_more _ = false

  let file_parser = None

  let toplevel_parser = Parser.toplevel Lexer.lexeme

  (** The command that actually executes a command. It accepts an argument which we can
      ignore, a flag indicating whether we are in ineractive mode, an environment, and a
      command to be excuted. It must return the new environment. *)
  let exec _ interactive env cmd =
    match cmd with
      | Syntax.Expression e ->
        let n = Eval.eval env e in
          if interactive then print_endline (string_of_int n) ;
          env
      | Syntax.Definition (x, e) ->
        let n = Eval.eval env e in
          print_endline (x ^ " is defined") ;
          (x, n) :: env
end) ;;

Calc.main ()
