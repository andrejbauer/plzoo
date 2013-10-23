module Calc = Zoo.Toplevel(struct
  type toplevel = Syntax.command
  type environment = (string * int) list
  let name = "calc"
  let options = []
  let help_directive = None
  let initial_environment = []
  let prompt = "> "
  let more_prompt = "> "
  let read_more _ = false
  let file_parser = None
  let toplevel_parser = Parser.toplevel Lexer.lexeme

  let exec _ interactive env e =
    match e with
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
