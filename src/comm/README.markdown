A simple procedural langauge with integer arithmetic, local variables, conditional
statements, `while` loops and printing. Programs are compiled to simple (simulated)
machine code.

#### Abstract syntax

Integer expression `e`:

* variable (a string of letters)
* integer constant
* addition `e₁ + e₂`
* subtraction `e₁ - e₂`
* multiplication `e₁ * e₂`
* integer division `e₁ / e₂`
* remainder `e₁ % e₂`

Boolean expression `b`:

* boolean constants `true` and `false`
* conjunction `b₁ and b₂`
* disjunction `b₁ or b₂` 
* negation `not b`

Command `c`:

* no operation `skip`
* local variable declaration `new x := e in c`
* print expression `print e`
* assign a varible `x := e`
* sequence commands `c₁ ; c₂`
* loop `while b do c done`
* conditional statement `if b then c₁ else c₂ end`

#### Compilation to machine code

The language is compiled to simplified machine code. Use the `--code` command-line option
to see the compiled code.

The underlying machine has a fixed amount of RAM (configurable `--ram` command-line
option), a program, the program counter, and a stack pointer. The stack grows downards,
from the top of RAM towards the bottom.

