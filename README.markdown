# The Programming Languages Zoo

The Programming Languages Zoo, or *PL Zoo* for short, is a collection of implementations
of miniature programming languages which demonstrates various techniques used in
implementation of programming languages. It is a good starting point for those who would
like to implement their own programming language, or just learn how it is done.

The PL Zoo is open source, see the file `LICENSE.markdown` for licensing information.

## Prerequisites

To compile the code you will need:

* [OCaml](http://www.ocaml.org/) programming language, version 4 or later,
* [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good way to get started with OCaml is to use the OCaml pagackage manager
[OPAM](http://opam.ocaml.org/), through which menhir is available. Both OCaml and OPAM are
available through package managers on Linux and OS X, see instruction on the [OPAM web
site](http://opam.ocaml.org/doc/Install.html).

### Recommended: `ledit` or `rlwrap`

It is recommended that you install [ledit](http://pauillac.inria.fr/~ddr/ledit/) or
[rlwrap](https://github.com/hanslub42/rlwrap) command-line editing wrappers. Check your
package manager, it probably knows about them. They will be detected and automatically
used by the toplevel interactive loop.

## Installation

Get the source from GitHub:

    git clone git@github.com:andrejbauer/plzoo.git

or via the HTTP protocol

    git clone https://github.com/andrejbauer/plzoo.git

Then you should compile the languages:

    make all

You can also compile a single language with

    make <lang>

The compilation procedure will create native code executables. To generate bytecode use

    make BUILD=byte all

There is `make clean` if you want ot clean up.

## Usage

The languages are not really meant to be used. Rather, you should read the source code,
which is decorated with ample comments, and study it. Also, each language `lang` has its
own `README.markdown` and `example.lang` in the subdirectory `src/lang`.

Nevertheless, all the language are functioning miniature versions of real languages that
can be executed. You can:

1. Run the toplevel with `./lang.native`
2. Run files non-interactively with `./lang.native <file> <file> ...`
3. Load files and enter the toplevel `./lang.native -l <file> -l <file> ...`

## Contributing

Contributions are *very* welcome. They are great service to the community and 


If you would like to contribute to the project, please contact us through GitHub:

* If you discover a problem, open an issue about it.
* Even better, fix the problem and submit a pull request!
* If you would like to help but do not know how, have a look at open issues and volunteer
  to resolve one.
* If you have an idea for a new language, discuss it in an issue, or just go ahead if you
  can't wait.

We kindly ask that contributions to the repository follow a certain pattern. For example,
all the main programs are generated with the `Zoo.Main` functor found in `src/zoo.ml`. It
takes care of a number of things, such as command-line wrappers, standard command-line
options, loading of files, and running an interactive shell. It is best if you start by
copying one fo the existing languages and adapt it to your language.

