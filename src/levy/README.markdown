An implementation of [Paul Levy's](<a href="http://www.cs.bham.ac.uk/~pbl/") [call-by-push-value](http://www.cs.bham.ac.uk/~pbl/cbpv.html)
language. The language has the following constructs:
* integers with arithmetical operations `*`, `+`, `-``
* booleans with conditional statements and comparison `=`, `<` of integers
* functions
* general recursion (fixpoint operator)
* call-by-push-value features: `return`, `thunk`, `force`, sequencing, and `let` binding

Call-by-push-value is very precise about what gets evaluated when and can express both
call-by-value and call-by-name.

#### Example interaction

The file `example.levy` defines a number of examples, including the factorial
function. You can load it and try it as follows:

    $ -l ./levy.native example.levy
    return (4) : F int
    return (6) : F int
    return (-2) : F int
    return (5) : F int
    return (1) : F int
    <fun> : int -> F int
    <fun> : int -> F int
    <fun> : int -> F int
    return (42) : F int
    val x : U (F int) = <thunk>
    return (1) : F int
    return (64) : F int
    return (64) : F int
    val a : int = 5
    val b : int = 15
    val c : U (F int) = <thunk>
    val fact : U int -> F int = <thunk>
    return (5040) : F int
    levy -- programming languages zoo
    Type Ctrl-D to exit
