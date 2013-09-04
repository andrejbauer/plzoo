1. DESCRIPTION

Calc is an implementation of a simple arithmetical calculator. It is
part of the PL Zoo, see http://math.andrej.com/plzoo/

The language has the following construct:

* Integer constants

* Operations +, -, unary -, *, and /


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

   ocamlbuild calc.byte


5. USAGE

First compile the program. You may then run the calculator with

    ./calc.byte

If you built the native code version, this would be

    ./calc.native

Example interaction:

    Caculator. Press Ctrl-D to quit.
    > 2+2
    4
    > 101 * 111
    11211
    > [Ctrl-D]
    Good bye.
