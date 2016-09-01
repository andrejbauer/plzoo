module Lambda = Zoo.Main(struct
  let name = "lambda"

  type command = Input.toplevel

  type environment = Context.context

  let initial_environment = Context.empty_context

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.commandline Lexer.token)

  let options = []

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

  (** [exec interactive ctx cmd] executes the toplevel command [cmd] and
       prints the result if in interactive mode. *)
  let exec ctx {Zoo.data=d;loc} =
    match d with

    | Input.Expr e ->
      let e = Desugar.expr ctx.Context.names e in
      let e = Norm.norm ~eager:!eager ~deep:!deep ctx.Context.decls e in
        Format.printf "%t@." (Print.expr ctx.Context.names e) ;
        ctx

    | Input.Context ->
      ignore
        (List.fold_right
           (fun x k ->
             (match Context.lookup_definition k ctx with
               | None ->
                 Format.printf "#constant @[%s@];@." x
               | Some e ->
                 Format.printf "@[%s := %t@];@." x (Print.expr ctx.Context.names e)) ;
             k - 1)
           ctx.Context.names (List.length ctx.Context.names - 1)) ;
      ctx

    | Input.Eager b ->
      eager := b ;
      Format.printf "@[I will evaluate %s.@." (if !eager then "eagerly" else "lazily") ;
      ctx

    | Input.Deep b ->
      deep := b ;
      Format.printf "@[I will evaluate %s.@." (if !deep then "deeply" else "shallowly") ;
      ctx

    | Input.TopConstant x ->
       if List.mem x ctx.Context.names then Zoo.error ~loc "%s already exists" x ;
       Format.printf "%s is a constant.@." x ;
       Context.add_parameter x ctx

    | Input.TopDefine (x, e) ->
      if List.mem x ctx.Context.names then Zoo.error ~loc "%s already exists" x ;
      let e = Desugar.expr ctx.Context.names e in
        Format.printf "%s is defined.@." x ;
        Context.add_definition x e ctx

    | Input.Help ->
      print_endline help_text ; ctx

    | Input.Quit -> exit 0

  let read_more _ = false
end) ;;

Lambda.main ()
