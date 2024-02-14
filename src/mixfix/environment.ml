type t = {
    operators: Precedence.graph;
    context: (Syntax.name * Presyntax.htype) list;
    env: Interpret.environment;
  }
