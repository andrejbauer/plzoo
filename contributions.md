---
layout: default
---

## How to contribute

The source code adheres to standards that keep it uniform and demonstrate (what we beleive
to be) good programming practice. We kindly ask that all contributions to the repository
follow the established pattern, and follow these guidelines:

1. The name of the language should also be a valid OCaml module name.
2. The language source code for language `lang` resides in `src/lang`.
3. If the language is an extension of `lang` with feature `X`, call it `lang_X`.
4. Language `lang` should have a file `src/lang/example.lang` which shows how it is used.
5. Language `lang` should have a file `src/lang/README.markdown` which explains briefly what the language is about. You can also put examples in this file, but note that the web page will already link to `src/lang/example.lang` so there is no point in just copying that to the `README.markdown` file.
6. Instead of making one language with many features, make several languages each demonstrating a feature.
7. Use interfaces `.mli` files and comment all entries in the interface using the ocamldoc
   format `(** ... *)`.
8. The main program should use the `Zoo.Main` functor:

        module MyLanguage = Zoo.Main(struct .... end) ;;
        MyLanguage.main() ;;

    This will give your language line-editing capabilities (if `rlwrap` is installed)
    and a uniform user interface.
9. Do not use `open` to open modules, there are
   [better solutions](https://realworldocaml.org/v1/en/html/files-modules-and-programs.html)
   (the best one is to just not look for any shortcuts).

Remember: these are *guidelines* that can be broken for a good enough reason.
