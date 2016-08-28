module Levy = Zoo.Main(struct
  let name = "levy"
  type command = Syntax.toplevel
  type environment = (Syntax.name * Syntax.ctype) list * Interpret.environment
  let options = []
  let initial_environment = ([], [])
  let read_more _ = false
  let file_parser = Some (Parser.file Lexer.token)
  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  let exec (ctx, env) = function
    | Syntax.Expr e ->
        (* type check [e], evaluate, and print result *)
        let ty = Type_check.type_of ctx e in
        let v = Interpret.interp env e in
        let judgement_kind = if Type_check.is_ctype ty then "comp" else "val" in
        Zoo.print_info "%s %s = %s@." judgement_kind (Syntax.string_of_type ty) (Interpret.string_of_runtime v);
        (ctx, env)
    | Syntax.Def (x, e) ->
        (* type check [e], evaluate it and store *)
        let ty = Type_check.type_of ctx e in
        Type_check.check_vtype ty ;
        let v = Interpret.interp env e in
        Zoo.print_info "val %s : %s = %s@." x (Syntax.string_of_type ty) (Interpret.string_of_runtime v);
        ((x,ty)::ctx, (x,v)::env)
end) ;;

Levy.main ()
