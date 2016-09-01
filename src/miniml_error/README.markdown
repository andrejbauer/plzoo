Miniml+error is a simple implementation of an eager functional language in which
division by zero raises an `error` exception that aborts execution. There is no
way to intercept `error` or to throw it directly.

In all other respects the language is like MiniML. The implementation contains a
parser, type-checker, compiler, and abstract machine interpreter.

The language has the following constructs:

* Integers with arithmetic operations +, - *, and /.

* Division by zero causes an error.

* Booleans with conditional statement and comparison of integers
  (only = and <).

* Recursive functions and function application. The expression

    fun f (x : t) : s is e

  denotes a function of type t -> s which maps x to e. In e
  the function refers to itself as f.

* Toplevel definitions

    let x = e

  There are no local definitions.

Example interaction:

    miniML+Error> 3 + (if 5 < 6 then 10 else 100);;
    - : int = 13
    miniML+Error> let x = 14;;
    x : int = 14
    miniML+Error> let fact = fun f (n : int) : int is if n = 0 then 1 else n * f (n-1);;
    fact : int -> int = <fun>
    miniML+Error> fact 10;;
    - : int = 3628800
    miniML+Error> 12/4;;
    - : int = 3
    miniML+Error> 1/0;;
    - : int = error
