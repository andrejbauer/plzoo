An eager purely functional language with records and subtypes.  The
language has integers, booleans, recursive functions, and immutable
records.

#### Examples

The file example.sub defines addition and conjugation of complex numbers.
You can load it and try it as follows:

    $ ./sub.native example.sub 
    val u : {re : int, im : int} = {re = 1, im = 0}
    val conjugate : {re : int, im : int} -> {re : int, im : int} = <fun>
    val plus : {re : int, im : int} -> {re : int, im : int} -> {re : int, im : int} = <fun>
    - : {re : int, im : int} = {re = 2, im = 0}
    - : {re : int, im : int} = {re = 5, im = -7}
