module Sub = Zoo.Main(struct
  let name = "sub"
  type command = Syntax.toplevel_cmd
  type environment = (string * Syntax.ty) list * Syntax.environment
  let options = []
  let initial_environment = ([], [])
  let read_more _ = false
  let file_parser = Some (Parser.file Lexer.token)
  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  let rec exec (ctx, env) = function
    | Syntax.Expr e ->
        (* type check [e], evaluate, and print result *)
        let ty = Type_check.type_of ctx e in
        let v = Eval.eval env e in
        Zoo.print_info "- : %s = %s@." (Syntax.string_of_type ty) (Syntax.string_of_value v) ;
        (ctx, env)
    | Syntax.Def (x, e) ->
        (* type check [e], and store it unevaluated! *)
        let ty = Type_check.type_of ctx e in
        let v = Eval.eval env e in
        Zoo.print_info "val %s : %s = %s@." x (Syntax.string_of_type ty) (Syntax.string_of_value v) ;
        ((x,ty) :: ctx, (x,v) :: env)

end) ;;

Sub.main ()
