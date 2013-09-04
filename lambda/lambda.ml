module Lambda = Zoo.Toplevel(struct
  type toplevel = Input.toplevel

  type environment = Context.context

  let initial_environment = Context.empty_context

  let prompt = "# "

  let more_prompt = "  "

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Parser.toplevel Lexer.token

  let name = "lambda"

  let options = []

  let help_directive = Some "#help;"

  (** Do we evaluate eagerly? *)
  let eager = ref false

  (** Do we evaluate inside lambda abstraction? *)
  let deep = ref false

  (** The help text printed when [#help] is used. *)
  let help_text = "Toplevel directives:
<expr> ;                      evaluate <expr>
#lazy ;                       evaluate lazily (do not evaluate arguments)
#eager ;                      evaluate eagrly (evaluate arguments immediately)
#deep ;                       evaluate inside λ-abstraction
#shallow ;                    do not evaluate inside λ-abstraction
#constant x ... y ;           declare constants
#context ;                    print current definitions
#help ;                       print this help
#quit ;                       exit

Syntax:
^ x ... y . e                  λ-abstraction
e1 e2                          application
"

  (** [exec interactive (ctx, env) cmd] executes the toplevel command [cmd] and
       prints the result if in interactive mode. *)
  let exec interactive (ctx, env) d =
    match d with
    | Input.Expr e ->
      let e = Desugar.expr ctx.names e in
      let e = Norm.norm ~eager:!eager ~deep:!deep ctx.decls e in
        if interactive then Format.printf "    = %t@." (Print.expr ctx.names e) ;
        ctx
    | Input.Context ->
      ignore
        (List.fold_right
           (fun x k ->
             (match Context.lookup_definition k ctx with
               | None ->
                 Format.printf "#constant @[%s@];@." x
               | Some e ->
                 Format.printf "@[%s := %t@];@." x (Print.expr ctx.names e)) ;
             k - 1)
           ctx.names (List.length ctx.names - 1)) ;
      ctx
    | Input.Eager b ->
      eager := b ;
      Format.printf "@[I will evaluate %s.@." (if !eager then "eagerly" else "lazily") ;
      ctx
    | Input.Deep b ->
      deep := b ;
      Format.printf "@[I will evaluate %s.@." (if !deep then "deeply" else "shallowly") ;
      ctx
    | Input.TopConstant xs ->
      List.fold_left
        (fun ctx x ->
          if List.mem x ctx.names then Error.typing ~loc "%s already exists" x ;
          if interactive then Format.printf "%s is a constant.@." x ;
          add_parameter x ctx)
        ctx xs
    | Input.TopDefine (x, e) ->
      if List.mem x ctx.names then Error.typing ~loc "%s already exists" x ;
      let e = Desugar.expr ctx.names e in
        if interactive then
          Format.printf "%s is defined.@." x ;
        add_definition x e ctx
    | Input.Help ->
      print_endline help_text ; ctx
    | Input.Quit -> exit 0


  let read_more str =
    let i = ref (String.length str - 1) in
      while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
      str.[!i]
end) ;;

Lambda.main ()
