module HM = Zoo.Main (struct

    let name = "HM"

    type command = Syntax.command

    let options = []

    type environment = {
      ty : Type.Env.env ;
      name: Syntax.Rename.env ;
      value: Eval.env ;
    }
    let add_def x ty v env = {
      ty = Type.Env.add x ty env.ty ;
      name = Syntax.Rename.add x.name x env.name ;
      value = Eval.add x v env.value ;
    }
    let initial_environment = {
      ty = Type.Env.empty;
      name = Syntax.Rename.SMap.empty ;
      value = Eval.initial_env ;
    }

    let read_more str = 
      let i = ref (String.length str - 1) in
      while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
      !i < 1 || (str.[!i] <> ';' || str.[!i - 1] <> ';')

    let file_parser = Some (Parser.file Lexer.token)
    let toplevel_parser = Some (Parser.toplevel Lexer.token)

    let exec env c =
      let c = Syntax.Rename.command env.name c in
      match c with
      | Syntax.Def (x, e) ->
        let ty = Typing.infer_top env.ty e in
        let v = Eval.execute env.value e in
        Zoo.print_info "@[<2>%a@ : @[%a@]@ = @[%a@]@."
          Printer.name x  Printer.typ ty  Printer.value v ;
        add_def x ty v env
  end)

let () = HM.main ()
