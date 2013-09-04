---
layout: default
title: Programming Languages Zoo
---


The *Programming Languages Zoo* implements a variety of programming languages. The implementations are purposely kept simple and to the point so that they are a good starting point for those who would like to implement their own programming language, or just learn how it is done.

The PL Zoo is open source, see the file `LICENSE.txt` for licensing information.

## Prerequisites

To compile the PL Zoo you will need:

* [OCaml](http://www.ocaml.org/) programming language, version 4 or later,
* [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.
* The `make` program.

Optionally, you should install `ledit` or `rlwrap` command-line editing wrappers. If you do, the language toplevels will automatically use them.

## Compilation

To build everything type `make`. You can also compile a specific language `X` by typing `make X.native`.

## WARNING

At the moment the PL Zoo is undergoing a major reorganization. I am porting it to Github
and consolidating the various programming languages into a more unified library. Please
stay tuned.

The following languages have been ported to the new toplevel interface:

* `lambda`
* `miniml`
* `calc`
