---
layout: default
navigation: installation
---

The zoo is available as a [GitHub project](https://github.com/andrejbauer/plzoo). Here
are detailed installation instructions.

## Prerequisites

To compile the code you will need:

* [OCaml](http://www.ocaml.org/) programming language, version 4 or later,
* [dune](https://dune.build) OCaml build system,
* [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good way to get started with OCaml is to use the OCaml pagackage manager [OPAM](http://opam.ocaml.org/), through which
menhir and dune are available. Both OCaml and OPAM are available through package managers on Linux and OS X, see
instruction on the [OPAM web site](http://www.ocaml.org/).

### Recommended: `ledit` or `rlwrap`

It is recommended that you install [ledit](http://pauillac.inria.fr/~ddr/ledit/) or
[rlwrap](https://github.com/hanslub42/rlwrap) command-line editing wrappers. Check your
package manager, it probably knows about them. They will be detected and automatically
used by the toplevel interactive loop.

## Learning OCaml

The languages are implemented in OCaml. If you are not familiar with OCaml, we highly
recommend that you look at the
[excellent resources for learning OCaml](https://ocaml.org/learn/).

## Obtaining the source code

Get the source from GitHub:

    git clone git@github.com:andrejbauer/plzoo.git

or via the HTTP protocol

    git clone https://github.com/andrejbauer/plzoo.git

If you do not use GitHub (why not?!) you can directly download a [ZIP archive](https://github.com/andrejbauer/plzoo/archive/master.zip).

## Compilation

To compile all the languages run:

    dune build

You can also compile a single language `lang` with

    dune build src/lang
