A simple implementation of [Paul Levy's](<a href="http://www.cs.bham.ac.uk/~pbl/") [call-by-push-value](http://www.cs.bham.ac.uk/~pbl/cbpv.html)
language. The language has the following constructs:
* integers with arithmetical operations `*`, `+`, `-``
* booleans with conditional statements and comparison `=`, `<` of integers
* functions
* general recursion (fixpoint operator)
* call-by-push-value features: `return`, `thunk`, `force`, sequencing, and `let` binding

Call-by-push-value is very precise about what gets evaluated when and can express both call-by-value and call-by-name.

#### Example interaction

The file `example.levy` defines a number of examples, including the factorial
function. You can load it and try it as follows:

    $ ./levy.native example.levy
    val int = 4
    val int = 6
    val int = -2
    comp F int = return 5
    comp F int = return 1
    comp int -> F int = <fun>
    comp int -> F int = <fun>
    comp int -> F int = <fun>
    comp F int = return 42
    val U F int = <thunk>
    comp F int = return 1
    comp F int = return 64
    comp F int = return 64
    val a : int = 5
    val b : int = 15
    val c : U F int = <thunk>
    val fact : U (int -> F int) = <thunk>
    comp F int = return 5040
