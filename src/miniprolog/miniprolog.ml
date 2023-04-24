module MiniProlog = Zoo.Main(struct
  let name = "miniProlog"
  type command = Syntax.toplevel_cmd
  type environment = unit
  let options = []
  let initial_environment = ()
  let file_parser = Some (fun _ -> Parser.file Lexer.token)
  let toplevel_parser = Some (fun _ -> Parser.expr Lexer.token)

  let exec () = function
    | Syntax.Goal g -> Solve.solve_toplevel g
    | Syntax.Assert a ->  Solve.assertz a

end) ;;

MiniProlog.main ()
