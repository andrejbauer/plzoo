module Levy = Zoo.Main(struct
  let name = "levy"
  type command = Syntax.toplevel
  type environment = (Syntax.name * Type_check.vtype) list * Eval.environment
  let options = []
  let initial_environment = ([], [])
  let read_more _ = false
  let file_parser = Some (Parser.file Lexer.token)
  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  let exec (ctx, env) = function
    | Syntax.Expr e ->
        (* type check [e], evaluate, and print result *)
        let ty = Type_check.ctype_of ctx e in
        let v = Eval.comp env e in
        Zoo.print_info "%t : %t@."
                       (Eval.print_value v)
                       (Type_check.print_ctype ty) ;
        (ctx, env)
    | Syntax.Def (x, e) ->
        (* type check [e], evaluate it and store *)
        let ty = Type_check.vtype_of ctx e in
        let v = Eval.expr env e in
        Zoo.print_info "val %s : %t = %t@." x 
                       (Type_check.print_vtype ty)
                       (Eval.print_value v);
        ((x,ty)::ctx, (x,v)::env)
end) ;;

Levy.main ()
