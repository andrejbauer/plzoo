---
layout: default
title: Programming Languages Zoo
---

The Programming Languages Zoo, or *PL Zoo* for short, is a collection of implementations
of miniature programming languages which demonstrates various techniques used in
implementation of programming languages. It is a good starting point for those who would
like to implement their own programming language, or just learn how it is done.

The languages demonstrate various aspects of implementation, such as:

* source code parsing (using a parser generator)
* pretty-printing of error messages and results
* how to report source-code positions
* interactive toplevel shell
* non-interactive file processing
* typed and untyped languages
* eager and lazy evaluation strategies
* subtyping, parametric polymorphism, and other kinds of type systems
* type checking and type inference
* functional, declarative, object-oriented, and procedural languages
* recursive definitions
* exceptions
* compilation to an assembly-like language
* abstract machine
* interpreter
* unification

## Installation

The PL Zoo is available as a [GitHub project](https://github.com/andrejbauer/plzoo). Here
are detailed installation instructions.

### Prerequisites

To compile the code you will need:

* [GNU Make](https://www.gnu.org/software/make/), which you probably have already
* [OCaml](http://www.ocaml.org/) programming language, version 4 or later,
* [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good way to get started with OCaml is to use the OCaml pagackage manager
[OPAM](http://opam.ocaml.org/), through which menhir is available. Both OCaml and OPAM are
available through package managers on Linux and OS X, see instruction on the [OPAM web
site](http://opam.ocaml.org/doc/Install.html).

#### Recommended: `ledit` or `rlwrap`

It is recommended that you install [ledit](http://pauillac.inria.fr/~ddr/ledit/) or
[rlwrap](https://github.com/hanslub42/rlwrap) command-line editing wrappers. Check your
package manager, it probably knows about them. They will be detected and automatically
used by the toplevel interactive loop.

### Learning OCaml

The PL Zoo is implemented in OCaml. If you are not familiar with OCaml, we highly
recommend that you look at the
[excellent resources for learning OCaml](https://ocaml.org/learn/).

### Obtaining the source code

Get the source from GitHub:

    git clone git@github.com:andrejbauer/plzoo.git

or via the HTTP protocol

    git clone https://github.com/andrejbauer/plzoo.git

You can also download a ZIP archive containing the latest version.

### Compilation

To compile all the languages run:

    make all

You can also compile a single language with

    make <lang>

The compilation procedure will create native code executables. To generate bytecode use

    make BUILD=byte all

There is `make clean` if you want ot clean up.

## Usage

The languages are not really meant to be used. Rather, you should read and study the
source code, which is decorated with ample comments. Also, each language `lang` has its
own `README.markdown` and `example.lang` in the subdirectory `src/lang`.

Nevertheless, all the language are fully functioning miniature versions of real languages
and can be executed. For each language `lang` you can:

1. see what command-line options are available with

        ./lang.native --help

2. run the toplevel with

        ./lang.native

3. run files non-interactively with

        ./lang.native <file> <file> ...`

4. load files and enter the toplevel

        ./lang.native -l <file> -l <file> ...`

## Authors

* [Andrej Bauer](http://andrej.com/)
* [Matija Pretnar](http://matija.pretnar.info/)

## Licensing

The PL Zoo is open source and released under the permissive [MIT license](LICENSE.html).

## Contributing

New contributions are *very* welcome. If you would like to contribute to the project,
please contact us through the [GitHub project page](https://github.com/andrejbauer/plzoo):

* If you discover a problem, [open an issue](https://github.com/andrejbauer/plzoo/issues/new).
* Even better, fix the problem and [submit a pull request](https://github.com/andrejbauer/plzoo/compare)!
* If you would like to help but do not know how, have a look at
  [open issues](https://github.com/andrejbauer/plzoo/issues) and volunteer to resolve one.
* If you have an idea for a new language, we will be happy to take it in. Please note that all
  the languages are purposely kept simple for educational purposes.

We kindly ask that contributions to the repository follow the established pattern. For
example, all the main programs are generated with the `Zoo.Main` functor found in
`src/zoo.ml`. It takes care of a number of things, such as command-line wrappers, standard
command-line options, loading of files, and running an interactive shell. It is best if
you start by copying one fo the existing languages and adapt it to your language.
