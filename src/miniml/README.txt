1. DESCRIPTION

Miniml is a simple implementation of an eager functional language. The
implementation contains a parser, type-checker, compiler, and abstract
machine interpreter. It is part of the PL Zoo, see
http://math.andrej.com/plzoo/

The language has the following constructs:

* Integers with arithmetic operations +, - and *. There is no
  division, so that the language is safe (no runtime errors or
  exceptions).

* Booleans with conditional statement and comparison of integers
  (only = and <)

* Recursive functions and function application. The expression

    fun f (x : t) : s is e

  denotes a function of type t -> s which maps x to e. In e
  the function refers to itself as f.

* Toplevel definitions

    let x = e

  There are no local definitions.


2. AUTHOR

The author of the program is Andrej Bauer <Andrej.Bauer@andrej.com>.
See the file COPYRIGHT.txt for license information.


3. REQUIREMENTS

You need Objective Caml, http://caml.inria.fr/ version 3.10 or higher.

If you have an older version of Objective Caml you can still compile
the code by hand.


4. COMPILATION

To compile the program run the command

    make

For the native code version run

    make native

If you do not have the make utility, run

   ocamlbuild miniml.byte


5. USAGE

First compile the program. You may then run the interpreter with

    ./miniml.byte

If you built the native code version, this would be

    ./miniml.native

Example interaction:

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
