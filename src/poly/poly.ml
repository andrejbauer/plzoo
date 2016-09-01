module Poly = Zoo.Main(struct

  let name = "Poly"

  type command = Syntax.toplevel_cmd

  type environment = (string * Syntax.htype) list * Interpret.environment

  let print_depth = ref 100

  let options = [("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]

  let initial_environment = ([], [])

  let read_more _ = false

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  let rec exec (ctx, env) = function
    | Syntax.Expr e ->
      (* type check [e], evaluate, and print result *)
       let ty = Type_infer.type_of ctx e in
       let v = Interpret.interp env e in
       Zoo.print_info "- : %s = " (Syntax.string_of_type (Syntax.rename ty)) ;
       Interpret.print_result !print_depth v ;
       Zoo.print_info "@." ;
       (ctx, env)
    | Syntax.Def (x, e) ->
       (* type check [e], and store it unevaluated! *)
       let ty = Type_infer.type_of ctx e in
       Zoo.print_info "val %s : %s@." x (Syntax.string_of_type (Syntax.rename ty)) ;
       ((x,ty)::ctx, (x, ref (Interpret.VClosure (env,e)))::env)

end) ;;

Poly.main () ;;
