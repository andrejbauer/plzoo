type t = {
    operators: Precedence.graph;
    context: (Syntax.name * Presyntax.htype) list;
    env: Interpret.environment;
  }

  let debug = true

let dprintln a =
  if debug then print_endline a else ()