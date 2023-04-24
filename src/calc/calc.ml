module Calc = Zoo.Main(struct
  let name = "calc"

  type command = Syntax.expression

  type environment = unit

  let options = []

  let initial_environment = ()

  let file_parser = None

  let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.lexeme)

  let exec () e =
    let n = Eval.eval e in
    Zoo.print_info "%d@." n

end) ;;

Calc.main ()
