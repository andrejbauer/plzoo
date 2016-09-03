
A simple object-oriented language with eager evaluation, first-class functions, and
dynamic types. It is based on a notion of objects as extensible records. There are no
classes.

The language has the following features:

* integers with arithmetical operations `*`, `+`, `-`, `/`
* booleans with conditional statements and comparison `=`, `<` of integers
* first-class functions
* mutable objects as extensible records, i.e., an object `obj1` can be extended
  by object `obj2` to form the combined object `obj1 with obj2`.

An interesting feature of the language is that, since "everything is
an object", a value may behave simultaneously as an integer, boolean,
function, and a record. See examples below.

### Example

The file `example.boa` shows basic examples and implements recursion and lists using
objects.
