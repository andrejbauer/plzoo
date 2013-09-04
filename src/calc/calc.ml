module Calc = Zoo.Toplevel(struct
  type toplevel = Syntax.expression
  type environment = unit
  let name = "calc"
  let options = []
  let help_directive = None
  let initial_environment = ()
  let prompt = "> "
  let more_prompt = "> "
  let read_more _ = false
  let file_parser = None
  let toplevel_parser = Parser.toplevel Lexer.lexeme

  let exec _ interactive () e =
    let n = Eval.eval e in
      if interactive then print_endline (string_of_int n)
end) ;;

Calc.main ()