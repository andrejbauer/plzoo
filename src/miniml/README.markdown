An implementation of an eager statically typed functional language with
a compiler and an abstract machine.

The language has the following constructs:

* Integers with arithmetic operations `+`, `-` and `*`. (There is no
  division because the language has no exceptions.)
* Booleans with conditional statement and comparison of integers
  `=` and `<`.
* Recursive functions and function application. The expression

        fun f (x : t) : s is e

  denotes a function of type `t -> s` which maps `x` to `e`. In `e`
  the function refers to itself as `f`.

* Toplevel definitions

        let x = e

  There are no local definitions.

Example interaction, see also the file `example.miniml`:

    MiniML. Press Ctrl-D to exit.
    MiniML> 3 + (if 5 < 6 then 10 else 100) ;;
    - : int = 13
    MiniML> let x = 14 ;;
    x : int = 14
    MiniML> let fact = fun f (n : int) : int is if n = 0 then 1 else n * f (n-1) ;;
    fact : int -> int = <fun>
    MiniML> fact 10 ;;
    - : int = 3628800
    MiniML>
    Good bye.
