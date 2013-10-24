1. DESCRIPTION

Calc+var is an implementation of a simple arithmetical calculator with
variables. It is part of the PL Zoo, see http://math.andrej.com/plzoo/

The language has the following construct:

* Variables

* Integer constants

* Operations +, -, unary -, *, and /

* Definitions of variables

2. AUTHOR

The author of the program is Andrej Bauer <Andrej.Bauer@andrej.com>.
See the file COPYRIGHT.txt for license information.

3. USAGE

First compile the program. You may then run the calculator with

    ./calc+var.byte

If you built the native code version, this would be

    ./calc+var.native

Example interaction:

    Caculator. Press Ctrl-D to quit.
    > 2+2
    4
    > 101 * 111
    11211
    > [Ctrl-D]
    Good bye.
