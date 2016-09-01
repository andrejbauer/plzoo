1. DESCRIPTION

Boa is a simple object-oriented language with eager evaluation,
first-class functions, and dynamic types. It is based on a notion of
objects as extensible records. There are no classes.

The language has the following features:

* integers with arithmetical operations *, +, -, /

* booleans with conditional statements and comparison =, < of integers

* first-class functions

* mutable objects as extensible records, i.e., an object obj1 can be extended
  by object obj2 to form the combined object "obj1 with obj2".

An interesting feature of the language is that, since "everything is
an object", a value may behave simultaneously as an integer, boolean,
function, and a record. See examples below.


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

   ocamlbuild boa.byte


5. USAGE

First compile the program. You may then run the interpreter with

    ./boa.byte

If you built the native code version, this would be

    ./boa.native

The file primes.minhs defines the infinite list of prime numbers. You
can load it and try it as follows:

    Boa. Press Ctrl-D to exit.
    Boa> let x = 1 with (fun t -> t * t + 10)
    x = 1 with <fun>
    Boa> x + 10
    11
    Boa> x 2
    14
    Boa> let a = {u=5, v=7}
    a = {u = 5, v = 7}
    Boa> let b = a with {w=7}
    b = {u = 5, v = 7, w = 7}
    Boa> b.v := 100
    100
    Boa> a
    {u = 5, v = 100}
    Boa> b
    {u = 5, v = 100, w = 7}

Also see the included file list.boa for an example showing how to
implement lists.
