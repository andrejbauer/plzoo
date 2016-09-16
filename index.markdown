---
layout: default
---

## About the zoo

The Programming Languages Zoo is a collection of miniature programming languages which
demonstrates various concepts and techniques used in programming language design and
implementation. It is a good starting point for those who would like to implement their
own programming language, or just learn how it is done.

The following features are demonstrated:

* functional, declarative, object-oriented, and procedural languages
* source code parsing with a parser generator
* recoding of source code positions
* pretty-printing of values
* interactive shell (REPL) and non-interactive file processing
* untyped, statically and dynamically typed languages
* type checking and type inference
* subtyping, parametric polymorphism, and other kinds of type systems
* eager and lazy evaluation strategies
* recursive definitions
* exceptions
* interpreters and compilers
* abstract machine

## Installation

See the [installation & compilation instructions](install.html).

## The languages

The following languages are on display:

{% include language-list.markdown %}

## Usage

The languages are not really meant to be used. Rather, you should [read and study the
source code](https://github.com/andrejbauer/plzoo/tree/master/src), which is decorated with ample comments. Also, each language `lang` has its
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

## License

The project is open source and released under the permissive [MIT license](license.html).

## Contributing

New contributions are *very* welcome. If you would like to contribute to the project,
please contact us through the [GitHub project page](https://github.com/andrejbauer/plzoo):

* If you discover a problem, [open an issue](https://github.com/andrejbauer/plzoo/issues/new).
* Even better, fix the problem and [submit a pull request](https://github.com/andrejbauer/plzoo/compare)!
* If you would like to help but do not know how, have a look at
  [open issues](https://github.com/andrejbauer/plzoo/issues) and volunteer to resolve one.
* If you have an idea for a new language, we will be happy to take it in. Please note that all
  the languages are purposely kept simple for educational purposes.

Before you contibute a new langauge, please read these
[guidelines for contributing](contributions.html).
