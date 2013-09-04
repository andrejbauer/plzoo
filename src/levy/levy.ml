module Levy = Zoo.Toplevel(struct
  type toplevel = Syntax.toplevel
  type environment = (Syntax.name * Syntax.ctype) list * Interpret.environment
  let initial_environment = ([], [])
  let name = "levy"
  let options = []
  let help_directive = Some "#help;;"
  let prompt = "Levy> "
  let more_prompt = "      "
  let read_more _ = false
  let file_parser = Some (Parser.file Lexer.token)
  let toplevel_parser = Parser.toplevel Lexer.token

  let help_message = "Toplevel directives:
<expr> ;;             evaluate <expr>
let x = <expr> ;;     toplevel definition
#use \"<file>\";;     load <file>
#help;;               print this help
#quit;;               exit
"

  let exec use_file interactive (ctx, env) = function
    | Syntax.Expr e ->
        (* type check [e], evaluate, and print result *)
        let ty = Type_check.type_of ctx e in
        let v = Interpret.interp env e in
          if interactive then
            print_endline ((if Type_check.is_ctype ty then "comp " else "val ") ^
              Syntax.string_of_type ty ^ " = " ^ Interpret.string_of_runtime v) ;
          (ctx, env)
    | Syntax.Def (x, e) ->
        (* type check [e], evaluate it and store *)
        let ty = Type_check.type_of ctx e in
        Type_check.check_vtype ty ;
        let v = Interpret.interp env e in
          if interactive then
            print_endline ("val " ^ x ^ " : " ^ Syntax.string_of_type ty ^ " = " ^ Interpret.string_of_runtime v) ;
         ((x,ty)::ctx, (x,v)::env)
    | Syntax.Quit -> raise End_of_file
    | Syntax.Use fn -> use_file (ctx, env) (fn, interactive)
    | Syntax.Help ->
      print_endline help_message ;
      (ctx, env)
end) ;;

Levy.main ()
